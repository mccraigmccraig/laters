(ns laters.control.state
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.state.protocols :as state.p]))

(defmacro get-state
  ([m]
   `(state.p/-get-state ~m))
  ([]
   `(state.p/-get-state ~'this-monad##)))

(defmacro put-state
  ([m st]
   `(state.p/-put-state ~m ~st))
  ([st]
   `(state.p/-put-state ~'this-monad## ~st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple State context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype State []
  m.p/Monad
  (-type [m]
    [::State])
  (-bind [m mv f]
    (r/plain-runnable
     (fn [{st :monad.state/state}]
       (let [{st' :monad.state/state
              val :monad/val} (r/run mv {:monad.state/state st})]
         (r/run (f val) {:monad.state/state st'})))))
  (-return [m v]
    (r/plain-runnable
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val v })))
  state.p/MonadState
  (-get-state [m]
    (r/plain-runnable
     (fn [{st :monad.state/state}]
       {:monad.state/state st
        :monad/val st})))
  (-put-state [m st']
    (r/plain-runnable
     (fn [{st :monad.state/state}]
       { :monad.state/state st'
        :monad/val nil}))))

(def state-ctx (->State))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.runnable :as r])
  (require '[laters.control.state :as m.state])

  (r/run
   (m/mlet m.state/state-ctx
     [{foo :foo :as a} (m.state/get-state)
      b (m/return 3)
      _ (m.state/put-state (assoc a :bar (* foo b)))
      c (m.state/get-state)]
     (m/return [a b c]))
   {:monad.state/state {:foo 10}}))
