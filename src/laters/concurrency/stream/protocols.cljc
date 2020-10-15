(ns laters.concurrency.stream.protocols)

(defprotocol IStreamImpl
  (-type [_]
    "a keyword describing the type of stream")
  (-create [this]
    "an in/out stream")
  (-from-seq [this s]
    "return a stream with contents from s")
  (-generate [this f]
    "return a stream with contents determined by f"))

;; basic stream operations, not considering
;; error handling in function application and chunking

;; want something that we can implement on rxjava, vertx, core.async and
;; manifold, which is simple to implement and gives us enough power to
;; layer consistent fp composition, error-handling and chunking on top
(defprotocol IRawStream
  (-map
    [this f]
    [this f impl]
    "return a new stream containing the results of mapping f
     over each element")

  (-filter
    [this f]
    [this f impl]
    "return a new stream with elements filtered by f")

  (-reduce
    [this f]
    [this f initval]
    [this f initval impl]
    "return a prommise containing the result of reducing the plain
     values on the stream with f")

  (-transform
    [this xform]
    [this xform impl]
    "return a new stream containing the transform of the plain
     values on the stream")

  (-realize-each
    [this]
    [this impl]
    "return a new stream with any promises unwrapped"))
