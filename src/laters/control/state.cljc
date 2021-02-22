(ns laters.control.state
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.state.protocols :as state.p]))

(defmacro get-state
  ([m]
   `(state.p/-get-state ~m))
  ([]
   `(state.p/-get-state ~'this-context##)))

(defmacro put-state
  ([m st]
   `(state.p/-put-state ~m ~st))
  ([st]
   `(state.p/-put-state ~'this-context## ~st)))
