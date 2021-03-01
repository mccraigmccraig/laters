(ns promisefx.fx.state
  (:require
   [promisefx.fx.state.protocols :as state.p]
   [promisefx.fx.monad.protocols :as m.p]))

(defmacro get
  ([m]
   `(state.p/-get-state ~m))
  ([]
   `(state.p/-get-state ~'this-context##)))

(defmacro put
  ([m st]
   `(state.p/-put-state ~m ~st))
  ([st]
   `(state.p/-put-state ~'this-context## ~st)))

(defmacro swap
  ([m f])
  ([f]))

(defmacro gets
  ([m f]
   `(m.p/-bind
     (state.p/-get-state ~m)
     #(m.p/-return (~f %))))
  ([f]
   `(m.p/-bind
     (state.p/-get-state ~'this-context##)
     #(m.p/-return (~f %)))))
