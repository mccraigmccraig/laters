(ns laters.monoid
  (:require
   [laters.abstract.context.protocols :as ctx.p])
  (:import
   [clojure.lang
    IPersistentList
    IPersistentMap
    IPersistentVector]))

(deftype MapMonoidCtx []
  ctx.p/Monoid
  (-mempty [_] {})
  ctx.p/Semigroup
  (-mappend [_ sv sv']
    (let [[k v :as kv] sv']
      (if (some? kv)
        (update (or sv {}) k #(ctx.p/-mappend nil (or % []) v))
        sv))))

(def map-monoid-ctx (->MapMonoidCtx))

(deftype ListMonoidCtx []
  ctx.p/Monoid
  (-mempty [_] '())
  ctx.p/Semigroup
  (-mappend [_ sv sv']
    (conj (or sv '()) sv')))

(def list-monoid-ctx (->ListMonoidCtx))

(deftype VectorMonoidCtx []
  ctx.p/Monoid
  (-mempty [_] [])
  ctx.p/Semigroup
  (-mappend [_ sv sv']
    (conj (or sv '[]) sv')))

(def vector-monoid-ctx (->VectorMonoidCtx))

(extend-protocol ctx.p/Contextual
  IPersistentMap
  (-get-context [_] map-monoid-ctx)
  IPersistentVector
  (-get-context [_] vector-monoid-ctx)
  IPersistentList
  (-get-context [_] list-monoid-ctx))

(extend-protocol ctx.p/Semigroup
  nil
  (-mappend [s sv sv']
    (ctx.p/-mappend
     (ctx.p/-get-context sv)
     sv
     sv')))

(defn mappend
  ([sv sv']
   (ctx.p/-mappend nil sv sv'))
  ([ctx sv sv']
   (ctx.p/-mappend ctx sv sv'))
  ([ctx sv sv' & others]
   (reduce
    (fn [sv sv']
      (ctx.p/-mappend ctx sv sv'))
    (ctx.p/-mappend ctx sv sv')
    others)))

(defn mempty
  [ctx]
  (ctx.p/-mempty ctx))
