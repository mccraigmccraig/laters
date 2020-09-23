(ns laters.control.identity
  (:require
   [laters.abstract.monad :as m]))

(deftype Identity [lifter]
  m/Monad
  (-bind [m mv f]
    (m/lift
     lifter
     m
     (f (m/lift-untag lifter m mv))))
  (-return [m v]
    (m/tag m v)))

(def identity-ctx (Identity. nil))

(comment
  (m/mlet m.id/identity-ctx
    [a (return 1)
     b (return 2)]
    (return (+ a b))))
