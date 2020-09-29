(ns laters.abstract.lifter.protocols)

(defprotocol ILifter
  (-lift [_ m tmv])
  (-lift-untag [_ m tmv]))

(defprotocol IAtomicLifter
  (-register [_ from to lifter])
  (-deregister [_ from to]))
