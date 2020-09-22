(ns laters.control.maybe
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]))

(deftype Maybe [lifters]
  m/Monad
  (-bind [m wmv f]
    (let [mv (m/untag (m/-lift m wmv))]
      (if (some? mv)
        (f mv)
        (m/tag m nil))))
  (-return [m v]
    (m/tag m v))
  (-lift [m wmv]
    (m/lift m lifters wmv))
  m/MonadZero
  (-mzero [m]
    (m/tag m nil)))

(def maybe-lifters
  {m.id/identity-ctx identity})

(defmethod m/-lets (.getName Maybe)
  [_ m]
  `[~'nothing (fn [] (m/tag ~m nil))])

(def maybe-ctx (Maybe. maybe-lifters))


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
