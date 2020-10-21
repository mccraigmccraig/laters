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

(def states [::open ::closed ::drained :errored])

(def on-overflow [:stream.overflow/park
                  :stream.overflow/error
                  :stream.overflow/drop-oldest
                  :stream.overflow/drop-latest])

(defn initial-state
  []
  {:stream-state ::open
   :demand 0
   :handler nil
   :buffer (ArrayDeque.)
   :park-q (ArrayDeque.)
   :emit-q (ArrayDeque.)
   :emitting false
   :error nil})

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
      v
      (if (> (count park-q) 0)
        (recur (.poll park-q))
        ::none))))

(defn next-emit
  [park-q buffer emit-q]
  (cond
    ;; check queued emits first
    (> (count emit-q) 0)
    (.poll emit-q)

    ;; if there are no queued emits, then try the buffer
    ;; if space gets freed in the buffer, fill it from the
    ;; park-q
    (> (count buffer) 0)
    (let [n (.poll buffer)
          np (next-parked park-q)]
      (when (not= ::none np)
        (.add buffer np))
      n)

    ;; finally try parked puts which haven't timed out
    (> (count park-q) 0)
    (next-parked park-q)))

(defn do-emit-one
  "emit one - it doesn't really matter which thread
   this gets run on, or whether emit-ones from different
   ops get interleaved - the results should be the same"
  [sb]
  (locking (.-lock sb)
    (if (not (true? (-> sb .-state-a deref :emitting)))
      (let [state-a (.-state-a sb)
            {stream-state :stream-state
             demand :demand
             handler :handler
             buffer :buffer
             park-q :park-q
             emit-q :emit-q
             :as state} (deref state-a)]
        (try
          (swap! state-a assoc :emitting true)

          (if (and
               (= ::open stream-state)
               (some? handler)
               (> demand 0))

            ;; there is demand, but is there anything to emit
            (let [v (next-emit park-q buffer emit-q)]
              (if (= ::none v)
                [::none] ;; nothing to emit

                (do ;; an actual emit!
                  (when (< demand Integer/MAX_VALUE)
                    (swap! state-a update :demand dec))

                  (let [r (handler v)]
                    (if (promise/promise? r)
                      (promise/handle
                       r
                       (fn [success error]
                         (if (some? error)
                           [::error error]
                           [::emitted-one success]))
                       (.promise-impl sb))
                      [::emitted-one r])))))

            ;; no demand, handler or ::closed
            [::none])

          (catch Throwable e
            [::error e])
          (finally
            (swap! state-a assoc :emitting false))))

      ;; we're already emitting
      [::none])))

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
  (loop [r (do-emit-one sb)]
    (if (promise/promise? r)
      (promise/then
       r
       (fn [[k v]]
         (case k
           ::emitted-one (do-emit-one sb)
           ::none (promise/resolve! completion-p true)
           ::error (do (stream.p/-error! sb v)
                       (promise/reject! completion-p v))
           ::throwable (do (stream.p/-error! sb v)
                           (promise/reject! completion-p v)))))

      ;; trampoline to avoid blowing stack in case of sync handler
      (let [[k v] r]
        (case k
          ::emitted-one (do-emit-one sb)
          ::none (promise/resolve! completion-p true)
          ::error (do (stream.p/-error! sb v)
                      (promise/reject! completion-p v))
          ::throwable (do (stream.p/-error! sb v)
                          (promise/reject! completion-p v)))))))

(defn do-emit
  "if there's no executor then emit on the caller's thread,
   otherwise use the executor. emit order is guaranteed by
   the use of the emit-q"
  [sb completion-p]
  (if (some? (.-executor sb))
    (.execute #(do-emit-task sb completion-p))
    (do-emit-task sb completion-p)))



(deftype StreamBuffer [promise-impl
                       on-overflow
                       buffer-size
                       park-q-size
                       lock
                       executor
                       state-a]

  stream.p/IStreamBuffer
  (-request! [this n]
    (locking lock
      (swap! state-a update :demand #(+ % n))
      (do-emit this (promise/deferred))
      ;; should this return true ?
      true))

  stream.p/IWriteStream
  (-put! [this v] (stream.p/-put! this v nil nil))
  (-put! [this v timeout] (stream.p/-put! this v timeout nil))
  (-put! [this v timeout timeout-val]
    (locking lock
      (let [{stream-state :stream-state
             demand :demand
             handler :handler
             buffer :buffer
             park-q :park-q
             emit-q :emit-q
             error :error
             :as state} @state-a]
        (cond (and
               (= ::open stream-state)
               (empty? park-q)
               (empty? buffer)
               (some? handler)
               (> demand 0))
              (let [completion-p (promise/deferred promise-impl)]

                (.add emit-q v)
                (do-emit this completion-p)

                (promise/then
                 completion-p
                 (constantly true)))

              (and
               (= ::open stream-state)
               (empty? park-q)
               (or
                (< (count buffer) buffer-size)
                (and (> (count buffer) 0)
                     (or
                      (= on-overflow :stream.overflow/drop-latest)
                      (= on-overflow :stream.overflow/drop-oldest)))))
              (let [completion-p (promise/deferred promise-impl)]
                (cond
                  (< (count buffer) buffer-size)
                  (.add buffer v)

                  (= on-overflow :stream.overflow/drop-latest)
                  (do
                    (.pop buffer)
                    (.add buffer v))

                  (= on-overflow :stream.overflow/drop-oldest)
                  (do
                    (.poll buffer)
                    (.add buffer v)))

                (do-emit this completion-p)

                (promise/then
                 completion-p
                 (constantly true)))

              (and
               (= ::open stream-state)
               (= on-overflow :stream.overflow/park)
               (< (count park-q) park-q-size))
              (let [completion-p (promise/deferred promise-impl)
                    completion-p (if timeout
                                   (promise/timeout
                                    completion-p
                                    timeout
                                    timeout-val
                                    promise-impl)
                                   completion-p)]
                ;; not finished here yet
                (.add emit-q {:completion-p completion-p
                              :value v})
                (do-emit this (promise/deferred promise-impl))
                completion-p)

              (and
               (= ::open stream-state))
              (promise/rejected
               promise-impl
               (ex-info "full!" {:v v}))


              (= ::closed stream-state)
              (promise/resolved false)

              (= :errored stream-state)
              (promise/rejected error)))))
  (-error! [this err]
    (locking this
      (swap! state-a
             assoc
             :stream-state ::errored
             :error err)))
  (-close! [this]
    (locking this
      (swap! state-a
             assoc
             :stream-state ::closed))))


(defn stream-buffer
  [{promise-impl :promise-impl
    on-overflow :on-overflow
    buffer-size :buffer-size
    park-q-size :park-q-size
    executor :executor
    :or {promise-impl promesa/default-impl
         on-overflow :stream.overflow/park
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
