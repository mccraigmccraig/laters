(ns laters.control.maybe
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.identity :as m.id]))

(deftype Maybe [lifter]
  m.p/Monad
  (-type[m]
    [::Maybe])
  (-bind [m mv f]
    (let [mv (l/lift-untag lifter m mv)]
      (if (some? mv)
        (l/lift lifter m (f mv))
        (t/tag m nil))))
  (-return [m v]
    (t/tag m v))
  m.p/MonadZero
  (-mzero [m]
    (t/tag m nil)))

(defn -nothing
  [m]
  (t/tag m nil))

(defmacro nothing
  []
  `(-nothing ~'this-monad##))

(def maybe-lifter
  {m.id/identity-ctx identity})

(def maybe-ctx (Maybe. maybe-lifter))

(defmacro maybe-let
  [& body]
  `(m/mlet maybe-ctx
     ~@body))

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


  )
