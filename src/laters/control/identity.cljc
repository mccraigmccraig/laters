(ns laters.control.identity
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as l])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Identity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Identity []
  m.p/Monad
  (-type [m]
    [::Identity])
  (-bind [m mv f]
    (f mv))
  (-return [m v]
    v))

(def identity-ctx (Identity.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TaggedIdentity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tagged-type
  [^ITaggedCtx ctx]
  [::Tagged (m.p/-type (tag.p/-inner-ctx ctx))])

(defn ^ITaggedMv tagged-bind
  [^ITaggedCtx ctx ^ITaggedMv tmv tmf]
  (let [mv (t/untag tmv)
        tr (m.p/-bind (tag.p/-inner-ctx ctx) mv tmf)]
    ;; lift here
    tr))

(defn ^ITaggedMv tagged-return
  [^ITaggedCtx ctx v]
  (t/tag
   ctx
   (m.p/-return (tag.p/-inner-ctx ctx) v)))

(deftype TaggedIdentity [lifter]
  tag.p/ITaggedCtx
  (-inner-ctx [this] identity-ctx)

  m.p/Monad
  (-type [m]
    (tagged-type m))
  (-bind [m tmv tmf]
    (tagged-bind m tmv tmf))
  (-return [m v]
    (tagged-return m v)))

(def tagged-identity-ctx (TaggedIdentity. nil))


(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])

  (m/mlet m.id/identity-ctx
    [a (m/return 1)
     b (m/return 2)]
    (m/return (+ a b)))

  (m/mlet m.id/tagged-identity-ctx
    [a (m/return 1)
     b (m/return 2)]
    (m/return (+ a b))))
