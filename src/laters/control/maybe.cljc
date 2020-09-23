(ns laters.control.maybe
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]))

(deftype Maybe [lifter]
  m/Monad
  (-bind [m wmv f]
    (let [mv (m/lift-untag lifter m wmv)]
      (if (some? mv)
        (m/lift lifter m (f mv))
        (m/tag m nil))))
  (-return [m v]
    (m/tag m v))
  m/MonadZero
  (-mzero [m]
    (m/tag m nil)))

(defmethod m/-lets (.getName Maybe)
  [_ m]
  `[~'nothing (fn [] (m/tag ~m nil))])

(def maybe-lifter
  {m.id/identity-ctx identity})

(def maybe-ctx (Maybe. maybe-lifter))

(defmacro maybe-let
  [& body]
  `(m/mlet maybe-ctx
     ~@body))


(comment
  (m/mlet m.maybe/maybe-ctx
    [a (return 1)
     b (nothing)
     c (return 10)]
    (return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (return 1)
     b (return 5)
     c (return 10)]
    (return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (return 1)
     b (return 5)
     :when nil
     c (return 10)]
    (return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (return 1)
     b (return 5)
     :when true
     c (return 10)]
    (return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
    [a (m/mlet m.id/identity-ctx [a (return 10)] (return a))
     b (return 3)]
    (return (* a b)))


  )
