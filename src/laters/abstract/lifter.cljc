(ns laters.abstract.lifter
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.lifter.protocols :as p]
   [laters.abstract.tagged :as t]
   [laters.abstract.tagged.protocols :as t.p])
  (:import
   [clojure.lang IFn]))

(defn lifter?
  [l]
  (satisfies? p/ILifter l))

(declare match-lifter*)

(defn ^:private match-segment
  [lifters tc r]
  (cond

    ;; gotcha
    (and (empty? r)
         (lifter? (get lifters tc)))
    (get lifters tc)

    (not-empty r)
    (match-lifter* (get lifters tc) (next r))

    :else
    nil))

(defn ^:private match-lifter*
  [lifters [tc & r]]
  (cond

    (contains? lifters tc)
    (match-segment lifters tc r)

    (contains? lifters :type/*)
    (match-segment lifters :type/* r)

    ;; no match
    :else
    nil))

;; a lifter which has an atom of
;; {<to-ctx-type> {<from-ctx-type-cmpnt> {<from-ctx-type-cmpnt>  <lifter>}}}
;; permitting bi-directional lifts to
;; be established e.g. P[promesa]<->PRW[RxJava] or PRW[*]<->PRWS[*]
(defrecord AtomicLifterRegistry [lifters-a]
  p/ILifterRegistry
  (-match-lifter [_ to-ctx-type from-ctx-type]
    (match-lifter* (get @lifters-a to-ctx-type {})
                   from-ctx-type))
  (-register [_ to-ctx-type from-ctx-type lifter]
    (swap!
     lifters-a
     assoc-in
     (into [to-ctx-type] from-ctx-type)
     lifter))
  (-deregister [_ to-ctx-type from-ctx-type]
    (swap!
     lifters-a
     update-in
     [to-ctx-type]
     dissoc
     (first from-ctx-type))))

(defn register
  [lifter-registry to-ctx-type from-ctx-type lifter-fn]
  (p/-register lifter-registry to-ctx-type from-ctx-type lifter-fn))

(defn register-all
  [lifter-registry to-ctx from-ctx-type->lifter-map]
  (doseq [[from-ctx-type lifter-fn] from-ctx-type->lifter-map]
    (p/-register lifter-registry (m.p/-type to-ctx) from-ctx-type lifter-fn)))

(defn create-atomic-lifter-registry
  []
  (AtomicLifterRegistry. (atom {})))

;; let plain fns be used as lifters
(extend IFn
  p/ILifter
  {:-lift-untagged (fn [this mv]
                     (this mv))})

(defn lift-untag
  "lifts a TaggedMV into Monad m, returning
   an untagged MV"
  [lifter-registry m mv]
  (cond
    (= m (t.p/-tagged-ctx mv))
    (t/untag mv)

    (some? lifter-registry)
    (let [lifter (p/-match-lifter
                  lifter-registry
                  (m.p/-type m)
                  (m.p/-type (t.p/-tagged-ctx mv)))]
      (if (some? lifter)
        (p/-lift-untagged lifter mv)

        (throw
         (ex-info
          "map lifter: no lifter registered"
          {:from (m.p/-type (t.p/-tagged-ctx mv))
           :to (m.p/-type m)
           :mv mv}))))

    :else
    (throw
     (ex-info
      "no lifter registry"
      {:from (t.p/-tagged-ctx mv)
       :to m
       :tmv mv}))))

(defn lift
  "lifts a TaggedMV into Monad m, returning
   TaggedMV"
  [lifter-registry m tmv]
  (t/tag m (lift-untag lifter-registry m tmv)))
