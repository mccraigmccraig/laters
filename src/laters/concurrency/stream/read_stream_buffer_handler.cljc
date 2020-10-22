(ns laters.concurrency.stream.read-stream-buffer-handler
  (:require
   [laters.concurrency.promise :as promise]
   [laters.concurrency.promise.promesa :as promesa]
   [laters.concurrency.stream.protocols :as stream.p])
  (:import
   [java.util ArrayDeque]))

(defn initial-state
  []
  {::handle-q (ArrayDeque.)
   ::take-q (ArrayDeque.)
   ::stream-buffer nil})

(defn fulfil-one-take
  "fulfil a single take if possible"
  [take-q v]
  (if (> (count take-q) 0)
    (loop [{take-r ::take-r} (.poll take-q)]
      (if (true? (promise/resolve! take-r v))
        ::take-fulfilled
        (if (> (count take-q) 0)
          (recur (.poll take-q))
          ::no-take)))
    ::no-take))

(defn fulfil
  "fulfil as many take-q items from the handle-q as possible"
  [handle-q take-q]
  (if (> (count handle-q) 0)
    (loop [{handle-r ::handle-r
            value ::value} (.peek handle-q)]
      (if (= ::take-fulfilled (fulfil-one-take take-q value))
        (do
          (.poll handle-q)
          (if (> (count handle-q) 0)
            (recur (.peek handle-q))
            ::done))
        ::done))
    ::done))

(deftype ReadStreamBufferHandler [promise-impl
                                  take-q-size
                                  state-a]
  stream.p/IStreamBufferHandler
  (-handle [this sb v]
    (locking this
      (let [{handle-q ::handle-q
             take-q ::take-q} @state-a
            handle-r (promise/deferred promise-impl)]

        (if (= ::take-fulfilled (fulfil-one-take take-q v))
          (promise/resolve! handle-r true)

          (.add handle-q {::handle-r handle-r
                          ::value v}))

        handle-r)))
  (-close [this sb])
  (-error [this sb err])

  stream.p/IReadStream
  (-take! [this])
  (-take! [this timeout])
  (-take! [this timeout timeout-val])
  (-take! [this default-val timeout timeout-val]
    (locking this
      (let [{handle-q ::handle-q
             take-q ::take-q
             stream-buffer ::stream-buffer} @state-a
            take-r (promise/deferred promise-impl)
            take-r (if timeout
                     (promise/timeout take-r timeout timeout-val promise-impl)
                     take-r)]

        ;; if we have a stream-buffer, request another message
        (when (some? stream-buffer)
          (stream.p/-request! stream-buffer 1))

        (.add take-q {::take-r take-r})

        (fulfil handle-q take-q)

        take-r)))

  (-buffer [this buffer-size])
  (-buffer [this buffer-size overflow])

  (-transform [this xform])
  (-transform [this xform impl])

  (-reduce [this f])
  (-reduce [this f initval])
  (-reduce [this f initval impl]))
