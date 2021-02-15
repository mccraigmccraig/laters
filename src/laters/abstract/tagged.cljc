(ns laters.abstract.tagged
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as lifter])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]
   [laters.abstract.lifter.protocols ILifter]))

(defn ^ITaggedCtx ctx
  [mv]
  (tag.p/-tagged-ctx mv))

(defn ^ITaggedMv tag
  [^ITaggedCtx t-ctx inner-mv]
  (tag.p/-tag t-ctx inner-mv))

(defn untag
  [^ITaggedMv tmv]
  (tag.p/-inner-mv tmv))

(defn tagged-type
  [^ITaggedCtx ctx]
  [::Tagged (m.p/-type (tag.p/-inner-ctx ctx))])

;; a tagged plain value

(defrecord TaggedPlainMv [^ITaggedCtx ctx mv]
  tag.p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-inner-mv [_] mv))

(defn tagged-plain
  [ctx mv]
  (->TaggedPlainMv ctx mv))

(defn ^ITaggedMv tagged-bind
  [^ILifter lifter ^ITaggedCtx ctx ^ITaggedMv tmv tmf]
  (let [mv (untag tmv)
        tmv' (m.p/-bind (tag.p/-inner-ctx ctx) mv tmf)
        from-ctx (tag.p/-tagged-ctx tmv')]
    (if (= ctx from-ctx) ;; short-circuit no-work case
      tmv'
      (let [utmv' (untag tmv')
            utlmv' (lifter/lift lifter (m.p/-type ctx) (m.p/-type from-ctx) utmv')]
        (tag ctx utlmv')))))

(defn ^ITaggedMv tagged-return
  [^ITaggedCtx ctx v]
  (tag
   ctx
   (m.p/-return (tag.p/-inner-ctx ctx) v)))

;; TODO - tagged-join??  - can maybe be used for other tagged method impls
;; such as the catch and finally impls...
