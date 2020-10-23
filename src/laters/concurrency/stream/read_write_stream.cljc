(ns laters.concurrency.stream.read-write-stream
  (:require
   [laters.concurrency.stream.protocols :as stream.p]
   [laters.concurrency.stream.impl.stream-buffer :as buffer]
   [laters.concurrency.stream.impl.read-stream-buffer-handler :as handler])
  (:import
   [java.io Writer]))

(deftype ReadWriteStream [stream-buffer
                          buffer-handler]
  stream.p/IWriteStream
  (-put! [this v]
    (stream.p/-put! stream-buffer v))
  (-put! [this v timeout]
    (stream.p/-put! stream-buffer v timeout))
  (-put! [this v timeout timeout-val]
    (stream.p/-put! stream-buffer v timeout timeout-val))

  (-error! [this err]
    (stream.p/-error! stream-buffer err))
  (-close! [this]
    (stream.p/-close! stream-buffer))

  stream.p/IReadStream
  (-take! [this]
    (stream.p/-take! buffer-handler))
  (-take! [this timeout]
    (stream.p/-take! buffer-handler timeout))
  (-take! [this timeout timeout-val]
    (stream.p/-take! buffer-handler timeout timeout-val))
  (-take! [this default-val timeout timeout-val]
    (stream.p/-take! buffer-handler default-val timeout timeout-val)))

(defmethod print-method ReadWriteStream [rws ^Writer w]
  (let [print-state {:stream-buffer (.-stream-buffer rws)
                     :buffer-handler (.-buffer-handler rws)}]
    (.write w "<< read-write-stream: ")
    (.write w (prn-str print-state))
    (.write w " >>")))

(defn read-write-stream
  ([] (read-write-stream {}))
  ([{:as opts}]
   (let [b (buffer/stream-buffer opts)
         h (handler/read-stream-buffer-handler opts)]
     (stream.p/-set-handler b h)

     (->ReadWriteStream b h))))
