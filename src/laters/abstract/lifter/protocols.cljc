(ns laters.abstract.lifter.protocols)

(defprotocol ILifter
  (-lift-untagged [_ mv]
    "lift a tagged-mv, returning
     an untagged mv"))

(defprotocol ILifterRegistry
  (-match-lifter [_ to-type from-type]
    "match a lifter for a tagged mv")
  (-register [_ to-type from-type lifter])
  (-deregister [_ to-type from-type]))
