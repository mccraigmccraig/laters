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
     (fn [st]
       (let [{val :val st' :state} ((m/lift-untag lifter m wmv) st)]
         ((m/lift-untag lifter m (f val)) st')))))
  (-return [m v]
    (m/tag
     m
     (fn [st]
       {:val v :state st})))
  MonadState
  (-get-state [m]
    (m/tag m (fn [st] {:val st :state st})))
  (-put-state [m st']
    (m/tag m (fn [st] {:val nil :state st'}))))

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
   {:foo 10}))
