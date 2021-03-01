(ns promisefx.data.success-failure
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.extractable.protocols :as extractable.p]))

(defrecord Success [ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] v))

(defn success [ctx v]
  (->Success ctx v))

(defn success?
  [v]
  (instance? Success v))

(defrecord Failure [ctx e]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] e))

(defn failure [ctx e]
  (->Failure ctx e))

(defn failure?
  [v]
  (instance? Failure v))
