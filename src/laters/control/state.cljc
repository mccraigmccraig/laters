(ns laters.control.state
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]))

(defprotocol MonadState
  (-get-state [m])
  (-put-state [m st']))

(deftype State [lifter]
  m.p/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{st :monad.state/state}]
       (let [{st' :monad.state/state
              val :monad/val} ((m/lift-untag lifter m wmv)
                                   {:monad.state/state st})]
         ((m/lift-untag lifter m (f val)) {:monad.state/state st'})))))
  (-return [m v]
    (m/tag
     m
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val v })))
  MonadState
  (-get-state [m]
    (m/tag
     m
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val st})))
  (-put-state [m st']
    (m/tag
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
  [wmv state]
  ((m/untag wmv) state))

(comment
  (m.state/run-state
   (m/mlet m.state/state-ctx
     [{foo :foo :as a} (m.state/get-state)
      b (m/return 3)
      _ (m.state/put-state (assoc a :bar (* foo b)))
      c (m.state/get-state)]
     (m/return [a b c]))
   {:monad.state/state {:foo 10}}))
