(ns laters.abstract.tagged
  (:require
   [laters.abstract.tagged.protocols :as p]
   [laters.abstract.monad.protocols :as m.p])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

(defrecord TaggedMv [^ITaggedCtx ctx mv]
  p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-untagged-mv [_] mv))

(defn ctx
  [mv]
  (p/-tagged-ctx mv))

(defn ^ITaggedMv tag
  [^ITaggedCtx t-ctx mv]
  (TaggedMv. t-ctx mv))

(defn untag
  [^ITaggedMv tmv]
  (p/-untagged-mv tmv))
