(ns laters.abstract.tagged
  (:require
   [laters.abstract.tagged.protocols :as p])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

(defn ctx
  [mv]
  (p/-tagged-ctx mv))

(defn ^ITaggedMv tag
  [^ITaggedCtx t-ctx inner-mv]
  (p/-tag t-ctx inner-mv))

(defn untag
  [^ITaggedMv tmv]
  (p/-inner-mv tmv))

(defrecord TaggedPlainMv [^ITaggedCtx ctx mv]
  p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-inner-mv [_] mv))

(defn tagged-plain
  [ctx mv]
  (->TaggedPlainMv ctx mv))
