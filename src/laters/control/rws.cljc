(ns laters.control.rws
  (:require
   [laters.abstract.monad :as m]
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]))

;; reader+writer+state
(deftype RWS [lifter]
  m.p/Monad
  (-type [m]
    [::RWS])
  (-bind [m mv f]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (let [{w :monad.writer/output
              st' :monad.state/state
              v :monad/val} ((l/lift-untag lifter m mv)
                             {:monad.reader/env env
                              :monad.state/state st})

             {w' :monad.writer/output
              st'' :monad.state/state
              v' :monad/val} ((l/lift-untag lifter m (f v))
                              {:monad.reader/env env
                               :monad.state/state st'})]

         {:monad.writer/output ((fnil into []) w w')
          :monad.state/state st''
          :monad/val v'}))))
  (-return [m v]
    (t/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val v})))

  m.r/MonadReader
  (-ask [m]
    (t/tag
     m
     (fn [{r :monad.reader/env st
          :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val r})))
  (-local [m f mv]
    (t/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       ((l/lift-untag lifter m mv)
        {:monad.reader/env (f r)
         :monad.state/state st}))))

  m.w/MonadWriter
  (-tell [m v]
    (t/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output [v]
        :monad.state/state st
        :monad/val nil})))
  (-listen [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val ((l/lift-untag lifter m mv)
                    {:monad.reader/env env})})))
  (-pass [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (let [{w :monad.writer/output
              st' :monad.state/state
              pass-val :monad/val} ((l/lift-untag lifter m mv)
                                    {:monad.reader/env env
                                     :monad.state/state st})
             [val f] (m.w/-as-vec pass-val)]
         {:monad.writer/output (f w)
          :monad.state/state st'
          :monad/val val}))))

  m.st/MonadState
  (-get-state [m]
    (t/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val st})))
  (-put-state [m st']
    (t/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st'
        :monad/val nil}))))

(def rws-lifters
  {[::m.id/Identity] (fn [mv]
                       (fn [{r :monad.reader/env
                            w :monad.writer/output
                            st :monad.state/state}]
                         {:monad.writer/output nil
                          :monad.state/state st
                          :monad/val (t/untag mv)}))})

(defn make-rws-ctx
  ([]
   (let [lifter-registry (l/create-atomic-lifter-registry)
         ctx (make-rws-ctx lifter-registry)]
     (l/register-all lifter-registry ctx rws-lifters)
     ctx))
  ([lifter-registry]
   (RWS. lifter-registry)))

(def rws-ctx (make-rws-ctx))

(defn run-rws
  [mv rws]
  ((t/untag mv) rws))


(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.rws :as m.rws])

  (m.rws/run-rws
   (m/mlet m.rws/rws-ctx
     [a (m/return 5)
      _ (m.writer/tell :foo)
      {b :bar} (m.reader/ask)
      c (m.reader/asks :foo)
      st (m.state/get-state)
      _ (m.state/put-state (assoc st :baz (+ a b c)))
      d (m.reader/local
         #(assoc % :bar 30)
         (m/mlet m.rws/rws-ctx
           [{a :foo b :bar} (m.reader/ask)]
           (m/return (+ a b))))
      _ (m.writer/tell d)]
     (m/return [a b c d]))
   {:monad.reader/env {:foo 20 :bar 10}
    :monad.state/state {:fip 12}})

  ;; auto-lifting
  (m.rws/run-rws
   (m/mlet m.rws/rws-ctx
     [a (m/mlet m.id/identity-ctx [a (m/return 10)] (m/return a))
      _ (m.writer/tell :foo)
      {b :bar} (m.reader/ask)
      st (m.state/get-state)
      _ (m.writer/tell st)
      _ (m.state/put-state (assoc st :baz (+ a b)))]
     (m/return (+ a b)))
   {:monad.reader/env {:bar 10}
    :monad.state/state {:fip 12}}))
