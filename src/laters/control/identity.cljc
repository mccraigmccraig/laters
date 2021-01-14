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

(def identity-ctx (->Identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TaggedIdentity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype TaggedIdentity [lifter]
  tag.p/ITaggedCtx
  (-inner-ctx [this] identity-ctx)
  (-tag [this inner-mv]
    (t/tagged-plain this inner-mv))

  m.p/Monad
  (-type [m]
    (t/tagged-type m))
  (-bind [m tmv tmf]
    (t/tagged-bind m tmv tmf))
  (-return [m v]
    (t/tagged-return m v)))

(def tagged-identity-ctx (->TaggedIdentity nil))

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
