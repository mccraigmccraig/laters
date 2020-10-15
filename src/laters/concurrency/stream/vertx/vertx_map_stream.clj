(ns laters.concurrency.stream.vertx.vertx-map-stream
  (:import
   [io.vertx.core Handler]
   [io.vertx.core.streams ReadStream]
   [io.vertx.core.streams.impl PipeImpl]))




(defn map-read-stream
  "return a new ReadStream with contents mapped by f"
  [^ReadStream src f]
  (reify ReadStream
    (endHandler [this handler]
      (.endHandler
       src
       (reify Handler
         (handle [_ event]
           (.handle handler event)))))
    (exceptionHandler [this handler]
      (.exceptionHandler
       src
       (reify Handler
         (handle [_ event]
           (.handle handler event)))))
    (fetch [this amount]
      (.fetch src amount))
    (handler [this handler]
      (.handler
       src
       (reify Handler
         (handle [_ event]
           (.handle handler (f event))))))
    (pause [this]
      (.pause src))
    (pipe [this]
      (PipeImpl. this))
    (pipeTo [this dst]
      (.pipeTo this dst nil))
    (pipeTo [this dst handler]
      (let [p (PipeImpl. this)]
        (.to dst handler)))
    (resume [this])))
