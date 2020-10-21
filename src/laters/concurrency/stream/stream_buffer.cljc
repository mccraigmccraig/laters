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

(defn initial-state
  [buffer-size park-q-size]
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

(defn do-emit-task
  "emit as much requested demand as is currently available"
  [sb completion-p]
  ;; re-entrancy protection - handling a message may
  ;; cause do-emit to be called again
  (when (not (true? (-> sb .-state-a deref :emitting)))
    (locking (.-lock sb)
      (let [state-a (.-state-a sb)
            {stream-state :stream-state
             demand :demand
             handler :handler
             emit-q :emit-q
             :as state} (deref state-a)]
        (try
          (swap! state-a assoc :emitting true)

          (while (and
                  (= ::open stream-state)
                  (some? handler)
                  (> demand 0)
                  (> (count emit-q) 0))
            (handler (.poll emit-q))
            (swap! state-a update :demand dec))

          (promise/resolve! completion-p true)

          (catch Exception e
            (stream.p/-error! sb e)
            (promise/reject! completion-p e))
          (catch Throwable e
            (stream.p/-error! sb e)
            (promise/reject! completion-p e)
            (throw e))
          (finally
            (swap! state-a assoc :emitting false)))))))

(defn do-emit
  "if there's no executor then emit on the caller's thread,
   otherwise use the executor. emit order is guaranteed by
   the use of the emit-q"
  [sb completion-p]
  (if (some? (.-executor sb))
    (.execute #(do-emit-task sb completion-p))
    (do-emit-task sb completion-p)))



(deftype StreamBuffer [promise-impl
                       buffer-size
                       park-q-size
                       lock
                       executor
                       state-a]

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
               (< (count buffer) buffer-size))
              (let [completion-p (promise/deferred promise-impl)]
                (.add buffer v)
                (do-emit this completion-p)
                (promise/then
                 completion-p
                 (constantly true)))

              (and
               (= ::open stream-state)
               (< (count park-q) park-q-size))
              (let [completion-p (promise/deferred promise-impl)]
                ;; not finished here yet
                (.add emit-q {:promise completion-p
                              :value v
                              :timeout timeout
                              :timeout-val timeout-val })
                (do-emit this (promise/deferred promise-impl))
                completion-p)

              (and
               (= ::open stream-state))
              (promise/rejected
               promise-impl
               (ex-info "park-q full" {:v v}))


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
    executor :executor
    buffer-size :buffer-size
    park-q-size :park-q-size
    :or {promise-impl promesa/default-impl
         executor nil
         buffer-size 0
         park-q-size 16384}}]
  (let [st (initial-state buffer-size park-q-size)]
    (->StreamBuffer
     promise-impl
     buffer-size
     park-q-size
     (Object.)
     executor
     (atom st))))
