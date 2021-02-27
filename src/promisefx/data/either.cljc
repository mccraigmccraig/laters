(ns promisefx.data.either
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.extractable.protocols :as extractable.p])
  (:import
   [promisefx.context.protocols Context]))

(defrecord Left [^Context ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] v))

(defn left
  ([ctx] (left ctx nil))
  ([ctx v]
   (->Left ctx v)))

(defn left?
  [v]
  (instance? Left v))

(defrecord Right [^Context ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] v))

(defn right
  [ctx v]
  (->Right ctx v))

(defn right?
  [v]
  (instance? Right v))
