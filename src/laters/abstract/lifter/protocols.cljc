(ns laters.abstract.lifter.protocols)

(defprotocol ILifter
  (-lift-untag [_ mv]))

(defprotocol ILifterRegistry
  (-match-lift-untag [_ m mv])
  (-register [_ to-type from-type lifter])
  (-deregister [_ to-type from-type]))
