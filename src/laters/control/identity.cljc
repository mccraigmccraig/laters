(ns laters.control.identity
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]))

(deftype Identity [lifter]
  m.p/Monad
  (-bind [m mv f]
    (l/lift
     lifter
     m
     (f (l/lift-untag lifter m mv))))
  (-return [m v]
    (t/tag m v)))

(def identity-ctx (Identity. nil))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])

  (m/mlet m.id/identity-ctx
    [a (m/return 1)
     b (m/return 2)]
    (m/return (+ a b))))
