(ns laters.concurrency.stream.stream-buffer
  (:require
   [laters.concurrency.promise :as promise]
   [laters.concurrency.promise.promesa :as promesa]
   [laters.concurrency.stream.protocols :as stream.p])
  (:import
   [java.util ArrayDeque]))

;; a buffer to implement parking puts

;; modelled after vertx io.vertx.core.streams.impl.InboundBuffer
;; https://github.com/eclipse-vertx/vert.x/blob/master/src/main/java/io/vertx/core/streams/impl/InboundBuffer.java

(def states [::open ::closed ::drained ::errored])

(def terminal-states #{::drained ::errored})
(def closed-or-terminal-states (conj terminal-states ::closed))

(def on-overflow [:stream.on-overflow/park
                  :stream.on-overflow/error
                  :stream.on-overflow/drop-oldest
                  :stream.on-overflow/drop-latest])

(defn initial-state
  []
  {::stream-state ::open
   ::demand 0
   ::handler nil
   ::buffer (ArrayDeque.)
   ::park-q (ArrayDeque.)
   ::emit-q (ArrayDeque.)
   ::emitting? false
   ::error nil})

;; for IWriteStream we'd need to implement buffering in addition to the
;; Publisher protocol (which only has subscribe), and a -put! method
;; which implements a promise-based park queue in addition to the buffer, so
;; -put! operates like:
;;
;; - if there is demand from a subscriber despatch immediately and
;;   return Promise<true>
;; - if there is space in the buffer add to the buffer and
;;   return Promise<true>
;; - if there is space in the wait-queue add to the wait-queue
;;   and return an unfulfilled Promise<...>
;;
;; - when demand is signalled, fulfil from the buffer, or from the wait-queue...
;;   when fulfilled from the wait-queue resolve the response promise of
;;   the waiter with <true>
;;
;; - when a timeout runs out on a wait-queue waiter, respond to the response
;;   promise with a TimeoutException or <timeout-val> if specified, and
;;   remove the watier from the wait-queue
;;
;; - when the stream errors signal the error to all subscribers and respond
;;   with the error to all wait-queue waiters
;;
;; - when a stream closes, accept no more -put! or -error!, but carry on
;;   delivering demand to consumers until the buffer and wait-queue are
;;   emptied, then signal onComplete to all subscribers. signal onComplete
;;   to any new subscribers

(defn next-parked
  [park-q]
  (loop [{completion-p :completion-p
          v :value} (.poll park-q)]
    (if (promise/resolve! completion-p true)
      ;; resolve! succeeded, so completion-p had not timed out
      v

      ;; resolve! failed, this put! had timed out... try for more
      (if (> (count park-q) 0)
        (recur (.poll park-q))
        ::none))))

(defn next-emit
  [park-q buffer emit-q]
  (cond
    ;; check queued emits first
    (> (count emit-q) 0)
    (.poll emit-q)

    ;; if there are no queued emits, try the buffer
    ;; if space gets freed in the buffer, fill it from the
    ;; park-q
    (> (count buffer) 0)
    (let [n (.poll buffer)
          np (next-parked park-q)]
      (when (not= ::none np)
        (.add buffer np))
      n)

    ;; finally try parked puts
    (> (count park-q) 0)
    (next-parked park-q)

    ;; nothing to emit
    :else
    ::none))

(defn do-emit-one
  "emit one - it doesn't really matter which thread
   this gets run on, or whether emit-ones from different
   ops get interleaved - the results should be the same"
  [sb]
  (locking (.-lock sb)
    ;; re-entrancy protection
    (if (not (true? (-> sb .-state-a deref ::emitting?)))
      (let [state-a (.-state-a sb)
            {stream-state ::stream-state
             demand ::demand
             handler ::handler
             buffer ::buffer
             park-q ::park-q
             emit-q ::emit-q
             error ::error
             :as state} (deref state-a)]
        (try
          (swap! state-a assoc ::emitting? true)

          (cond

            (and
             (not (contains? terminal-states stream-state))
             (some? handler)
             (> demand 0))
            ;; there is demand, but is there anything to emit
            (let [v (next-emit park-q buffer emit-q)]

              (if (= ::none v) ;; nothing to emit
                (do
                  (when (= ::closed stream-state)
                    (swap! state-a assoc ::stream-state ::drained))
                  [::done])

                (do ;; an actual emit!
                  (when (< demand Integer/MAX_VALUE)
                    (swap! state-a update ::demand dec))

                  (let [r (stream.p/-handle handler sb v)]
                    (if (promise/promise? r)
                      (promise/handle
                       r
                       (fn [success error]
                         (if (some? error)
                           [::error error]
                           [::emitted-one success]))
                       (.promise-impl sb))

                      [::emitted-one r])))))

            (= ::errored stream-state)
            [::error error]

            (= ::drained stream-state)
            [::done]

            :else
            (throw (ex-info "bad state" state)))

          (catch Throwable e
            [::error e])
          (finally
            (swap! state-a assoc ::emitting? false))))

      ;; we're already emitting
      [::done])))

(defn do-emit-task
  "emit as much requested demand as is currently available. keep on
   emitting until either demand is satisfied or there are no items to
   emit.
   if the handler returns a promise, then chain emission to resolution of
   that promise, if the handler returns sync then loop emission
   resolve the completion-p when done. "
  [sb completion-p]
  ;; re-entrancy protection - handling a message may
  ;; cause do-emit to be called again
  (try
    (loop [r (do-emit-one sb)]
      (if (promise/promise? r)
        (promise/handle
         r
         (fn [[k v] error]
           (if (some? error)
             ;; should never error, but better to pass it on if it does
             (promise/reject!
              completion-p
              error)
             (case k
               ::emitted-one (do-emit-task sb completion-p)
               ::done (promise/resolve! completion-p true)
               ::error (do (stream.p/-error! sb v)
                           (promise/reject! completion-p v))
               (promise/reject!
                completion-p
                (ex-info "bad case:" [k v])))))
         (.-promise-impl sb))

        ;; trampoline to avoid blowing stack in case of sync handler
        (let [[k v] r]
          (case k
            ::emitted-one (recur (do-emit-one sb))
            ::done (promise/resolve! completion-p true)
            ::error (do (stream.p/-error! sb v)
                        (promise/reject! completion-p v))
            (promise/reject!
             completion-p
             (ex-info "do-emit bad case:" {:state-buffer sb
                                           :kv [k v]}))))))
    (catch Throwable e
      (promise/reject!
       completion-p
       (ex-info "do-emit error" {:state-buffer sb})))))

(defn do-emit
  "if there's no executor then emit on the caller's thread,
   otherwise use the executor.
   returns completion-p, which will be resolved when the emit operation
   completes"
  [sb completion-p]
  (if (some? (.-executor sb))
    (.execute #(do-emit-task sb completion-p))
    (do-emit-task sb completion-p))
  completion-p)

(deftype StreamBuffer [promise-impl
                       on-overflow
                       buffer-size
                       park-q-size
                       lock
                       executor
                       state-a]

  stream.p/IStreamBuffer
  (-request! [this n]

    ;;TODO what to do if we are not ::open

    (locking lock
      (swap! state-a update ::demand #(+ % n))
      (do-emit this (promise/deferred))
      ;; should this return true ?
      true))

  stream.p/IWriteStream
  (-put! [this v] (stream.p/-put! this v nil nil))
  (-put! [this v timeout] (stream.p/-put! this v timeout nil))
  (-put! [this v timeout timeout-val]
    (locking lock
      (let [{stream-state ::stream-state
             demand ::demand
             handler ::handler
             buffer ::buffer
             park-q ::park-q
             emit-q ::emit-q
             error ::error
             :as state} @state-a]
        (cond (and
               (= ::open stream-state)
               (empty? park-q)
               (empty? buffer)
               (some? handler)
               (> demand 0))
              ;; emit straight to the handler
              (let [completion-p (promise/deferred promise-impl)]
                (.add emit-q v)
                (do-emit this completion-p)
                completion-p)

              (and
               (= ::open stream-state)
               (empty? park-q)
               (or
                (< (count buffer) buffer-size)
                (and (> (count buffer) 0)
                     (or
                      (= on-overflow :stream.on-overflow/drop-latest)
                      (= on-overflow :stream.on-overflow/drop-oldest)))))
              ;; buffer, maybe with overflow behaviour
              (let [completion-p (promise/deferred promise-impl)]
                (cond
                  (< (count buffer) buffer-size)
                  (.add buffer v)

                  (= on-overflow :stream.on-overflow/drop-latest)
                  (do
                    (.pop buffer)
                    (.add buffer v))

                  (= on-overflow :stream.on-overflow/drop-oldest)
                  (do
                    (.poll buffer)
                    (.add buffer v)))
                (do-emit this completion-p)
                completion-p)

              (and
               (= ::open stream-state)
               (= on-overflow :stream.on-overflow/park)
               (< (count park-q) park-q-size))
              ;; park
              (let [completion-p (promise/deferred promise-impl)
                    completion-p (if timeout
                                   (promise/timeout
                                    completion-p
                                    timeout
                                    timeout-val
                                    promise-impl)
                                   completion-p)]

                (.add emit-q {:completion-p completion-p
                              :value v})
                ;; throw away the emit completion - the response
                ;; completes when the value gets added to the buffer
                ;; or handled
                (do-emit this (promise/deferred promise-impl))

                completion-p)

              (and
               (= ::open stream-state))
              ;; no space even to park - reject and error the sream
              (do
                (let [err (ex-info "full!" {:v v})]
                  (stream.p/-error! this err)
                  (promise/rejected promise-impl err)))

              (= ::closed stream-state)
              ;; stream is closed
              (promise/resolved false)

              (= ::errored stream-state)
              ;; stream is errored
              (promise/rejected error)

              :else
              (promise/rejected (ex-info "unknown stream-state"
                                         {::stream-state stream-state}))))))
  (-error! [this err]
    (locking lock
      (let [{stream-state ::stream-state} @state-a]
        (when (not (contains? terminal-states stream-state))
          (swap! state-a
                 assoc
                 ::stream-state ::errored
                 ::error err)
          (when-let [handler (-> state-a deref ::handler)]
            (stream.p/-error handler this err))))))
  (-close! [this]
    (locking lock
      (let [{stream-state ::stream-state} @state-a]
        (when (not (contains? closed-or-terminal-states stream-state))
          (swap! state-a
                 assoc
                 ::stream-state ::closed)
          (when-let [handler (-> state-a deref ::handler)]
            (stream.p/-close handler this))))))
  (-set-handler [this handler]
    (locking lock
      (if handler
        (if (some? (-> state-a deref ::handler))
          (throw (ex-info "handler already set" {:stream-buffer this}))
          (do
            (swap! state-a assoc ::handler handler)
            (let [{stream-state ::stream-state
                   err ::error} @state-a]
              (case stream-state
                ::closed (do-emit this (promise/deferred promise-impl))
                ::drained (stream.p/-close handler this)
                ::errored (stream.p/-error handler this err)
                ::open nil
                (throw (ex-info "unknown stream-state" {::stream-state stream-state})))
              true)))
        (swap! state-a assoc ::handler nil)))))


(defn stream-buffer
  [{promise-impl :stream/promise-impl
    on-overflow :stream/on-overflow
    buffer-size :stream/buffer-size
    park-q-size :stream/park-q-size
    executor :stream/executor
    :or {promise-impl promesa/default-impl
         on-overflow :stream.on-overflow/park
         buffer-size 0
         park-q-size 16384
         executor nil}}]
  (let [st (initial-state)]
    (->StreamBuffer
     promise-impl
     on-overflow
     buffer-size
     park-q-size
     (Object.)
     executor
     (atom st))))
