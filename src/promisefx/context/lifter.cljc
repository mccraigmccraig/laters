(ns promisefx.context.lifter
  (:require
   [promisefx.context.protocols :as p])
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

(extend-protocol p/ILifter
  nil
  (-lift [_ to-type from-type utmv]
    (if (= to-type from-type)
      utmv
      (throw (ex-info "no lift" {:to-type to-type
                                 :from-type from-type
                                 :utmv utmv})))))

(defn lift
  "lifts an untagged mv from one type to another"
  [lifter to-type from-type utmv]
  (p/-lift lifter to-type from-type utmv))
