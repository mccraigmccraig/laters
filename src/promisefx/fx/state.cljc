(ns promisefx.fx.state
  (:require
   [promisefx.fx.state.protocols :as state.p]))

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
