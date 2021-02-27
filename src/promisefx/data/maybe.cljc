(ns promisefx.data.maybe
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.extractable.protocols :as extractable.p])
  (:import
   [promisefx.context.protocols Context]))

(defrecord Just [^Context ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
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
  extractable.p/Extract
  (-extract [_] nil))

(defn nothing
  [ctx]
  (->Nothing ctx))

(defn nothing?
  [v]
  (instance? Nothing v))
