(ns laters.abstract.lifter.protocols)

(defprotocol ILifter
  (-lift-untagged [_ umv]
    "lift an untagged-mv, returning
     an untagged mv"))

(defprotocol ILifterRegistry
  (-match-lift-untag [_ m mv]
    "match a lifter for a tagged mv, returning
     an untagged mv")
  (-register [_ to-type from-type lifter])
  (-deregister [_ to-type from-type]))
