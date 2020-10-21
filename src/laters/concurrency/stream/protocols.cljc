(ns laters.concurrency.stream.protocols)

(defprotocol IStreamImpl
  (-type [_]
    "a keyword describing the type of stream")
  (-create
    [this]
    [this buffer-size]
    [this buffer-size overflow]
    "create an IWriteStream+IReadStream
     - default buffer-size is 0
     - default backpressure stratety is :stream.overflow/park")
  (-from-seq [this s]
    "return an IReadStream with contents from s")
  )

;; basic stream operations, not considering
;; error handling in function application and chunking

;; want something that we can implement on rxjava, vertx, core.async and
;; manifold, which is simple to implement and gives us enough power to
;; layer consistent fp composition, error-handling and chunking on top

;; for rxjava IReadStream seems feasible - although -take! doesn't really
;; make sense for Observables, only Flowables, and it's slightly awkward there
;; (subscribe a new subscriber, in onSubscribe fetch 1 and terminate
;; the subscription in onNext) ... but that's probably ok,
;; -take! isn't used heavily... could also have a take-subscriber which
;; stays subscribed and only signals demand after a -take! call
;;
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

(defprotocol IWriteStream
  (-put!
    [this v]
    [this v timeout]
    [this v timeout timeout-val]
    "put a value on the stream
     - returns a promise of the accept status
     - true if the value was accepted
     - false if the value was not accepted (because the stream is closed)
     - timeout-val if the put timed out
     - an error if the stream is errored or no timeout-val was supplied")
  (-error!
    [this err]
    "signal an error and put the stream in a terminal errored state")
  (-close!
    [this]
    "signal the stream is ended and put it in a terminal state"))

(defprotocol IStreamBuffer
  (-request!
    [this n]
    "request the delivery of n objects"))

(defprotocol IReadStream
  (-take!
    [this]
    [this timeout]
    [this timeout timeout-val]
    [this default-val timeout timeout-val]
    "take from the stream
     - returns a promise of the value
     - timeout-val if the read times out (and timeout-val was supplised)
     - default-val if the stream is closed
     - an if the stream errored or no timeout-val was supplied")

  (-buffer
    [this buffer-size]
    [this buffer-size overflow]
    "backpressure is one of
      - :stream.overflow/park (default)
      - :stream.overflow/error
      - :stream.overflow/drop-oldest
      - :stream.overflow/drop-latest")

  (-transform
    [this xform]
    [this xform impl]
    "return a new stream containing the transform of the plain
     values on the stream")

  (-reduce
    [this f]
    [this f initval]
    [this f initval impl]
    "return a prommise containing the result of reducing the plain
     values on the stream with f")
  )
