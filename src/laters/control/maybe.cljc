(ns laters.control.maybe
  (:require
   [laters.abstract.context.protocols :as ctx.p])
  (:import
   [laters.abstract.context.protocols Context]))

(defrecord Just [^Context ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] v))

(defn just
  [ctx v]
  (->Just ctx v))

(defn just?
  [v]
  (instance? Just v))

(defrecord Nothing [^Context ctx]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] nil))

(defn nothing
  [ctx]
  (->Nothing ctx))

(defn nothing?
  [v]
  (instance? Nothing v))
