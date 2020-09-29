(ns laters.control.maybe
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]))

(deftype Maybe [lifter]
  m.p/Monad
  (-bind [m wmv f]
    (let [mv (m/lift-untag lifter m wmv)]
      (if (some? mv)
        (m/lift lifter m (f mv))
        (m/tag m nil))))
  (-return [m v]
    (m/tag m v))
  m.p/MonadZero
  (-mzero [m]
    (m/tag m nil)))

(defn -nothing
  [m]
  (m/tag m nil))

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
