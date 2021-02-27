(ns promisefx.data.monoid
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.monoid.protocols :as monoid.p])
  (:import
   [clojure.lang
    IPersistentList
    IPersistentMap
    IPersistentVector]))

(defn mappend
  ([sv sv']
   (monoid.p/-mappend nil sv sv'))
  ([ctx sv sv']
   (monoid.p/-mappend ctx sv sv'))
  ([ctx sv sv' & others]
   (reduce
    (fn [sv sv']
      (monoid.p/-mappend ctx sv sv'))
    (monoid.p/-mappend ctx sv sv')
    others)))

(defn mempty
  [ctx]
  (monoid.p/-mempty ctx))

(deftype MapMonoidCtx []
  monoid.p/Monoid
  (-mempty [_] {})
  monoid.p/Semigroup
  (-mappend [ctx sv sv']
    (cond
      (sequential? sv')
      (let [[k v :as kv] sv']
        (if (some? kv)
          (update (or sv {}) k #(monoid.p/-mappend nil (or % []) v))
          sv))

      (map? sv')
      (merge-with
       #(apply mappend nil (or %1 []) %2)
       sv
       sv')

      nil?
      sv

      :else
      (throw (ex-info "can't mappend" {:ctx ctx :sv sv :sv' sv'})))))

(def map-monoid-ctx (->MapMonoidCtx))

(deftype ListMonoidCtx []
  monoid.p/Monoid
  (-mempty [_] '())
  monoid.p/Semigroup
  (-mappend [_ sv sv']
    (conj (or sv '()) sv')))

(def list-monoid-ctx (->ListMonoidCtx))

(deftype VectorMonoidCtx []
  monoid.p/Monoid
  (-mempty [_] [])
  monoid.p/Semigroup
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

(extend-protocol monoid.p/Semigroup
  nil
  (-mappend [s sv sv']
    (monoid.p/-mappend
     (ctx.p/-get-context sv)
     sv
     sv')))
