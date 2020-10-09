(ns laters.control.promise.protocols)

(defprotocol IPromiseCtx
  (-promise-impl [_]))
