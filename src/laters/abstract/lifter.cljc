(ns laters.abstract.lifter
  (:require
   [laters.abstract.lifter.protocols :as p]
   [laters.abstract.tagged :as t]
   [laters.abstract.tagged.protocols :as t.p])
  (:import
   [clojure.lang Associative]))

(defn ^:private assoc-lift*
  [lifters m tmv]
  (if (contains? lifters (t.p/-ctx tmv))
    ((get lifters (t.p/-ctx tmv)) (t/untag tmv))

    (throw
     (ex-info
      "map lifter: no lifter registered"
      {:from (t.p/-ctx tmv)
       :to m
       :tmv tmv}))))

;; a plain map {<from-ctx> <lifter>} can be used to provide lifters
;; for a single context...
(extend Associative
  p/ILifter
  {:-lift-untag assoc-lift*
   :-lift (fn [this m tmv]
            (t/tag m (assoc-lift* this m tmv)))})

;; a lifter which has an atom of
;; {<to-ctx> {<from-ctx> <lifter>}}
;; permitting bi-directional lifts to
;; be established e.g. P<->PRW or PRW<->PRWS
(defrecord AtomicLifter [lifters-a]
  p/ILifter
  (-lift-untag [_ m tmv]
    (assoc-lift* (get @lifters-a m {}) m tmv))
  (-lift [_ m tmv]
    (t/tag m (assoc-lift* (get @lifters-a {}) m tmv)))
  p/IAtomicLifter
  (-register [_ to-ctx from-ctx lifter]
    (swap!
     lifters-a
     assoc-in
     [to-ctx from-ctx]
     lifter))
  (-deregister [_ to-ctx from-ctx]
    (swap!
     lifters-a
     update-in
     to-ctx
     dissoc
     from-ctx)))

(defn register
  [lifter to-ctx from-ctx lifter-fn]
  (p/-register lifter to-ctx from-ctx lifter-fn))

(defn register-all
  [lifter to-ctx from-lifter-map]
  (doseq [[from-ctx lifter-fn] from-lifter-map]
    (p/-register lifter to-ctx from-ctx lifter-fn)))

(defn create-atomic-lifter
  []
  (AtomicLifter. (atom {})))

(defn lift-untag
  "lifts a TaggedMV into Monad m, returning
   an untagged MV"
  [lifter m tmv]
  (cond
    (= m (t.p/-ctx tmv))
    (t/untag tmv)

    (some? lifter)
    (p/-lift-untag lifter m tmv)

    :else
    (throw
     (ex-info
      "no lifts"
      {:from (t.p/-ctx tmv)
       :to m
       :tmv tmv}))))

(defn lift
  [lifter m tmv]
  (t/tag m (lift-untag lifter m tmv)))
