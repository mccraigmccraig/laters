(ns laters.control.identity
  (:require
   [laters.abstract.monad :as m]))

(deftype Identity []
  m/Monad
  (-bind [m mv f]
    (f (m/untag mv)))
  (-return [m v]
    (m/tag m v))
  (-lift [m wmv]
    (m/lift nil m wmv)))

(def identity-ctx (Identity.))


(comment
  (m/mlet m.id/identity-ctx
    [a (return 1)
     b (return 2)]
    (return (+ a b))))
