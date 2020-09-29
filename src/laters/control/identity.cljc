(ns laters.control.identity
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]))

(deftype Identity [lifter]
  m.p/Monad
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
    [a (m/return 1)
     b (m/return 2)]
    (m/return (+ a b))))
