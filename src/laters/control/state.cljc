(ns laters.control.state
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]))

(defprotocol MonadState
  (-get-state [m])
  (-put-state [m st']))

(deftype State [lifter]
  m.p/Monad
  (-type [m]
    [::State])
  (-bind [m mv f]
    (t/tag
     m
     (fn [{st :monad.state/state}]
       (let [{st' :monad.state/state
              val :monad/val} ((l/lift-untag lifter m mv)
                               {:monad.state/state st})]
         ((l/lift-untag lifter m (f val)) {:monad.state/state st'})))))
  (-return [m v]
    (t/tag
     m
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val v })))
  MonadState
  (-get-state [m]
    (t/tag
     m
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val st})))
  (-put-state [m st']
    (t/tag
     m
     (fn [{st :monad.state/state}]
       { :monad.state/state st'
        :monad/val nil}))))

(defmacro get-state
  ([m]
   `(-get-state ~m))
  ([]
   `(-get-state ~'this-monad##)))

(defmacro put-state
  ([m st]
   `(-put-state ~m ~st))
  ([st]
   `(-put-state ~'this-monad## ~st)))

(def state-ctx (State. nil))

(defn run-state
  [mv state]
  ((t/untag mv) state))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.state :as m.state])

  (m.state/run-state
   (m/mlet m.state/state-ctx
     [{foo :foo :as a} (m.state/get-state)
      b (m/return 3)
      _ (m.state/put-state (assoc a :bar (* foo b)))
      c (m.state/get-state)]
     (m/return [a b c]))
   {:monad.state/state {:foo 10}}))
