(ns promisefx.data.extractable.protocols)

(defprotocol Extract
  (-extract [_]
    "extract a value from a context"))

;; extract arbitrary values to themselves
(extend-protocol Extract
  nil
  (-extract [self] self)
  Object
  (-extract [self] self))
