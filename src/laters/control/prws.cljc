(ns laters.control.prws
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]
   [laters.control.promise :as m.pr]
   [promesa.core :as p]))


;; ({:monad.reader/env r :monad.state/state st})->Promise<{:monad/val v :monad.writer/output w :monad.state/state st}
(deftype PRWS [lifter]
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((m/lift-untag lifter m wmv) {:monad.reader/env env
                                      :monad.state/state st})
        (fn [{w :monad.writer/output
             st' :monad.state/state
             v :monad/val}]
          (p/all [w ((m/lift-untag lifter m (f v))
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
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val v}))))
  m/MonadZero
  (-mzero [m]
    (m/tag
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
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val env}))))
  (-local [m f mv]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       ((m/lift-untag lifter m mv) {:monad.reader/env (f env)
                                    :monad.state/state st}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :monad.reader/env st :monad.state/state}]
       (p/resolved
        {:monad.writer/output [v] :monad.state/state st :monad/val nil}))))
  (-listen [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env
                                     :monad.state/state st})
        (fn [{w :monad.writer/output
             st' :monad.state/state
             v :monad/val
             :as lv}]
          {:monad.writer/output w
           :monad.state/state st'
           :monad/val lv})))))
  (-pass [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env
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
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val st}))))
  (-put-state [m st']
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st'
         :monad/val nil})))))

(defmethod m/-lets (.getName PRWS)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'asks (fn [f#] (m.r/-asks ~m f#))
    ~'local (fn [f# mv#] (m.r/-local ~m f# mv#))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))
    ~'pass (fn [mv#] (m.w/-pass ~m mv#))
    ~'get-state (fn [] (m.st/-get-state ~m))
    ~'put-state (fn [st'#] (m.st/-put-state ~m st'#))])

(def prws-lifter
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :monad.reader/env w :monad.writer/output st :monad.state/state}]
                         (p/resolved
                          {:monad.writer/output nil :monad.state/state st :monad/val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :monad.reader/env w :monad.writer/output st :monad.state/state}]
                        (p/chain
                         mv
                         (fn [v]
                           {:monad.writer/output nil :monad.state/state st :monad/val v}))))})

(def prws-ctx (PRWS. prws-lifter))

(defn run-prws
  [wmv rws]
  ((m/untag wmv) rws))

(comment

  @(m/run-prws
    (m/mlet m.prws/prws-ctx
      [{a :foo} (ask)
       b (asks :bar)
       c (get-state)
       _ (put-state a)
       d (return (+ a b c))
       _ (tell d)
       e (m/mlet m.pr/promise-ctx
           [a (return 100)
            b (return 100)]
           (return (* a a)))
       f (local
          #(assoc % :bar 100)
          (m/mlet m.prws/prws-ctx
            [{a :foo b :bar} (ask)]
            (return (+ a b))))]
      (return [a b c d e f]))
    {:monad.reader/env {:foo 10 :bar 20}
     :monad.state/state 50})
  )
