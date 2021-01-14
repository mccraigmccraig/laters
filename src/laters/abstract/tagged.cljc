(ns laters.abstract.tagged
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged.protocols :as tag.p])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

(defn ctx
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
  p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-inner-mv [_] mv))

(defn tagged-plain
  [ctx mv]
  (->TaggedPlainMv ctx mv))

(defn ^ITaggedMv tagged-bind
  [^ITaggedCtx ctx ^ITaggedMv tmv tmf]
  (let [mv (untag tmv)
        tr (m.p/-bind (tag.p/-inner-ctx ctx) mv tmf)]
    ;; lift here
    tr))

(defn ^ITaggedMv tagged-return
  [^ITaggedCtx ctx v]
  (tag
   ctx
   (m.p/-return (tag.p/-inner-ctx ctx) v)))
