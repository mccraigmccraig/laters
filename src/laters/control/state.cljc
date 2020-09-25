(ns laters.control.state
  (:require
   [laters.abstract.monad :as m]))

(defprotocol MonadState
  (-get-state [m])
  (-put-state [m st']))

(deftype State [lifter]
  m/Monad
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

(defmethod m/-lets (.getName State)
  [_ m]
  `[~'get-state (fn [] (-get-state ~m))
    ~'put-state (fn [st'#] (-put-state ~m st'#))])

(def state-ctx (State. nil))

(defn run-state
  [wmv state]
  ((m/untag wmv) state))

(comment
  (m/run-state
   (m/mlet m.state/state-ctx
     [{foo :foo :as a} (get-state)
      b (return 3)
      _ (put-state (assoc a :bar (* foo b)))
      c (get-state)]
     (return [a b c]))
   {:monad.state/state {:foo 10}}))
