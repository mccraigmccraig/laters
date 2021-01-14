(ns laters.control.maybe
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as l]
   [laters.control.identity :as m.id])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Maybe context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Maybe []
  m.p/Monad
  (-type[m]
    [::Maybe])
  (-bind [m mv f]
    (when (some? mv)
      (f mv)))
  (-return [m v]
    v)
  m.p/MonadZero
  (-mzero [m]
    nil))

(defn -nothing
  [m]
  (m.p/-mzero m))

(defmacro nothing
  []
  `(-nothing ~'this-monad##))

(defn make-maybe-ctx
  []
  (Maybe.))

(def maybe-ctx (make-maybe-ctx))

;; (def maybe-lifters
;;   {[::m.id/Identity] t/untag})

;; (defn make-maybe-ctx
;;   ([]
;;    (let [lifter-registry (l/create-atomic-lifter-registry)
;;          ctx (make-maybe-ctx lifter-registry)]
;;      (l/register-all lifter-registry ctx maybe-lifters)
;;      ctx))
;;   ([lifter-registry]
;;    (Maybe. lifter-registry)))


(defmacro maybe-let
  [& body]
  `(m/mlet maybe-ctx
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Maybe context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^ITaggedMv tagged-zero
  [^ITaggedCtx ctx]
  (m.p/-mzero (tag.p/-inner-ctx ctx)))

(deftype TaggedMaybe []
  tag.p/ITaggedCtx
  (-inner-ctx [this] maybe-ctx)
  (-tag [this inner-mv]
    (t/tagged-plain this inner-mv))

  m.p/Monad
  (-type [m]
    (m.id/tagged-type m))
  (-bind [m tmv tmf]
    (m.id/tagged-bind m tmv tmf))
  (-return [m v]
    (m.id/tagged-return m v))

  m.p/MonadZero
  (-mzero [m]
    (tagged-zero m)))

(def tagged-maybe-ctx (TaggedMaybe.))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.maybe :as m.maybe])

  (m/mlet m.maybe/maybe-ctx
    [a (m/return 1)
     b (m.maybe/nothing)
     c (m/return 10)]
    (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (m/return 1)
     b (m/return 5)
     c (m/return 10)]
    (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (m/return 1)
     b (m/return 5)
     :when nil
     c (m/return 10)]
    (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (m/return 1)
     b (m/return 5)
     :when true
     c (m/return 10)]
    (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (m/mlet m.id/identity-ctx [a (m/return 10)] (m/return a))
     b (m/return 3)]
    (m/return (* a b)))


  (m/mlet m.maybe/tagged-maybe-ctx
    [a (m/return 1)
     b (m/return 5)
     :when true
     c (m/return 10)]
    (m/return (+ a b c)))
  )
