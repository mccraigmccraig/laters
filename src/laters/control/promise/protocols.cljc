(ns laters.control.promise.protocols)

(defprotocol IPromiseFactory
  (-resolved [ctx v])
  (-rejected [ctx err]))

(defprotocol IPromise
  (-then [p f])
  (-handle [p f]))
