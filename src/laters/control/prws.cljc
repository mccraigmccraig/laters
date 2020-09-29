(ns laters.control.prws
  (:require
   [laters.abstract.monad :as m]
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]
   [laters.control.promise :as m.pr]
   [promesa.core :as p]))


;; ({:monad.reader/env r :monad.state/state st})->Promise<{:monad/val v :monad.writer/output w :monad.state/state st}
(deftype PRWS [lifter]
  m.p/Monad
  (-bind [m wmv f]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((l/lift-untag lifter m wmv) {:monad.reader/env env
                                      :monad.state/state st})
        (fn [{w :monad.writer/output
             st' :monad.state/state
             v :monad/val}]
          (p/all [w ((l/lift-untag lifter m (f v))
                     {:monad.reader/env env
                      :monad.state/state st'})]))
        (fn [[w {w' :monad.writer/output
                st'' :monad.state/state
                v' :monad/val}]]
          (p/resolved
           {:monad.writer/output ((fnil into []) w w')
            :monad.state/state st''
            :monad/val v'}))))))
  (-return [m v]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val v}))))
  m.p/MonadZero
  (-mzero [m]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/rejected
        (ex-info
         ":mopr.control.monad/mzero"
         {:monad.writer/output [::mzero]
          :monad.state/state st
          :monad/val nil})))))

  m.r/MonadReader
  (-ask [m]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val env}))))
  (-local [m f mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       ((l/lift-untag lifter m mv) {:monad.reader/env (f env)
                                    :monad.state/state st}))))

  m.w/MonadWriter
  (-tell [m v]
    (t/tag
     m
     (fn [{r :monad.reader/env st :monad.state/state}]
       (p/resolved
        {:monad.writer/output [v] :monad.state/state st :monad/val nil}))))
  (-listen [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((l/lift-untag lifter m mv) {:monad.reader/env env
                                     :monad.state/state st})
        (fn [{w :monad.writer/output
             st' :monad.state/state
             v :monad/val
             :as lv}]
          {:monad.writer/output w
           :monad.state/state st'
           :monad/val lv})))))
  (-pass [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((l/lift-untag lifter m mv) {:monad.reader/env env
                                     :monad.state/state st})
        (fn [{w :monad.writer/output
             st' :monad.state/state
             pass-val :monad/val}]
          (let [[val f] (m.w/-as-vec pass-val)]
            {:monad.writer/output (f w)
             :monad.state/state st'
             :monad/val val}))))))

  m.st/MonadState
  (-get-state [m]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val st}))))
  (-put-state [m st']
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st'
         :monad/val nil})))))

(def prws-lifter
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :monad.reader/env
                            st :monad.state/state}]
                         (p/resolved
                          {:monad.writer/output nil
                           :monad.state/state st
                           :monad/val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :monad.reader/env
                           st :monad.state/state}]
                        (p/chain
                         mv
                         (fn [v]
                           {:monad.writer/output nil
                            :monad.state/state st
                            :monad/val v}))))})

(def prws-ctx (PRWS. prws-lifter))

(defn run-prws
  [wmv rws]
  ((t/untag wmv) rws))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.maybe :as m.maybe])
  (require '[laters.control.reader :as m.reader])
  (require '[laters.control.writer :as m.writer])
  (require '[laters.control.promise :as m.pr])
  (require '[laters.control.prws :as m.prws])

  @(m.prws/run-prws
    (m/mlet m.prws/prws-ctx
      [{a :foo} (m.reader/ask)
       b (m.reader/asks :bar)
       c (m.state/get-state)
       _ (m.state/put-state a)
       d (m/return (+ a b c))
       _ (m.writer/tell d)
       e (m/mlet m.pr/promise-ctx
           [a (m/return 100)
            b (m/return 100)]
           (m/return (* a a)))
       f (m.reader/local
          #(assoc % :bar 100)
          (m/mlet m.prws/prws-ctx
            [{a :foo b :bar} (m.reader/ask)]
            (m/return (+ a b))))]
      (m/return [a b c d e f]))
    {:monad.reader/env {:foo 10 :bar 20}
     :monad.state/state 50})
  )
