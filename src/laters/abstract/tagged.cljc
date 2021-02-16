(ns laters.abstract.tagged
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as lifter])
  (:import
   [laters.abstract.tagged.protocols ITagSource ITagged ITaggedCtx]))

(defn get-tag
  [^ITagSource v]
  (tag.p/-get-tag v))

(defn ^ITagged tag
  [^ITaggedCtx t-ctx v]
  (tag.p/-tag t-ctx v))

(defn untag
  [^ITagged tv]
  (tag.p/-get-value tv))

(defrecord Tagged [tag v]
  tag.p/ITagSource
  (-get-tag [_] tag)
  tag.p/ITagged
  (-get-value [_] v))

(defn tagged
  [tag v]
  (->Tagged tag v))

;; a Tagged context, for use as an inner-context with
;; IdentityTransformer
;; ErrorTransformer
;; RWErrorTransformer
;; PRWTransformer
;; etc

(deftype TaggedCtx [t lifter]
  tag.p/ITagSource
  (-get-tag [m] t)
  tag.p/ITaggedCtx
  (-tag [m v] (tagged t v))
  m.p/Monad
  (-type [m]
    [::Tagged t])
  (-bind [m mv f]
    (let [tmv' (f (untag mv))
          tmv'-tag (get-tag tmv')]
      (if (= t tmv'-tag)
        tmv'
        (let [mv' (lifter/lift lifter t tmv'-tag (untag tmv'))]
          (tagged t mv')))))
  (-return [m v]
    (tag m v)))

(def tagged-ctx (->TaggedCtx nil nil))
