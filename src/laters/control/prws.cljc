(ns laters.control.prws
  (:require
   [laters.abstract.monad :as m]
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.abstract.lifter.protocols :as l.p]
   [laters.abstract.error.protocols :as e.p]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]
   [laters.control.promise :as m.pr]
   [promesa.core :as p])
  (:import
   [java.util.concurrent ExecutionException CompletionException]
   [clojure.lang ExceptionInfo]
   [clojure.lang Atom]))

(defn unwrap-error
  "java.util.concurrent likes to wrap exceptions"
  [error]
  (if-let [cause (.getCause error)]
    cause
    error))

(defn is-prws-error?
  [error]
  (and (instance? ExceptionInfo error)
       (= ::error (-> error ex-data :type))
       (instance? Atom (-> error ex-data :monad.writer/output))
       (-> error ex-data (contains? :monad.state/state))))

(defn prws-error
  ([error] (prws-error error nil nil))
  ([error prior-state] (prws-error error nil prior-state))
  ([error prior-output prior-state]
   (ex-info
    (str ::error)
    {:type ::error
     :monad/error error
     :monad.writer/output (atom prior-output)
     :monad.state/state prior-state})))

(defn concat-error
  [error prior-output prior-state]
  (let [error (unwrap-error error)]
    (if (is-prws-error? error)

      (do
        (swap!
         (-> error ex-data :monad.writer/output)
         (fn [output]
           ((fnil into []) prior-output output)))

        error)

      (prws-error error prior-output prior-state))))

;; ({:monad.reader/env r :monad.state/state st})->Promise<{:monad/val v :monad.writer/output w :monad.state/state st}
(deftype PRWS [lifter]
  m.p/Monad
  (-bind [m wmv f]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (-> (m.pr/pcatch ((l/lift-untag lifter m wmv) {:monad.reader/env env
                                                      :monad.state/state st}))
           (p/handle
            (fn [{w :monad.writer/output
                 st' :monad.state/state
                 v :monad/val
                 :as success}
                error]
              (if (some? error)
                (do
                  (p/rejected
                   (concat-error error [] st)))

                (do
                  (p/handle
                   (m.pr/pcatch ((l/lift-untag lifter m (f v))
                                 {:monad.reader/env env
                                  :monad.state/state st'}))

                   (fn [{w' :monad.writer/output
                        st'' :monad.state/state
                        v' :monad/val
                        :as success}
                       error]
                     (if (some? error)

                       (do
                         (p/rejected
                          (concat-error error w st')))
                       (do
                         (p/resolved
                          {:monad.writer/output ((fnil into []) w w')
                           :monad.state/state st''
                           :monad/val v'})))))))))))))
  (-return [m v]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/resolved
        {:monad.writer/output nil
         :monad.state/state st
         :monad/val v}))))

  e.p/MonadError
  (-reject [m error]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (p/rejected
        (prws-error error st)))))
  (-catch [m handler mv]
    (t/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (-> (m.pr/pcatch ((l/lift-untag lifter m mv) {:monad.reader/env env
                                                     :monad.state/state st}))
           (p/handle
            (fn [success error]
              (if (some? error)
                (let [error (unwrap-error error)
                      {output-a :monad.writer/output
                       state :monad.state/state
                       cause :monad/error} (ex-data error)

                      output (some-> output-a deref)
                      val (handler cause)]
                  {:monad.writer/output output
                   :monad.state/state state
                   :monad/val val})

                success)))))))


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

(def prws-lifters
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

(def prws-lifter (l/create-atomic-lifter))

(def prws-ctx (PRWS. prws-lifter))

(l/register-all prws-lifter prws-ctx prws-lifters)

(m/deflets
  {prws-let laters.control.prws/prws-ctx})

(defn run-prws
  [wmv rws]
  ((t/untag wmv) rws))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.error :as e])
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

  ;; catch

  @(m.prws/run-prws
    (e/catch
        m.prws/prws-ctx
        ex-data
      (m.prws/prws-let
       (throw (ex-info "boo" {:foo 100}))))
    {})

  (def emv
    (m.prws/prws-let
     [a (m.reader/asks :foo)
      _ (m.writer/tell [:foo a])
      st (m.state/get-state)
      _ (m.state/put-state (assoc st :bar 20))
      _ (throw (ex-info "wah" {:a a :st st}))]
     (m/return [a st])))

  (def er
    (m.prws/run-prws
     emv
     {:monad.reader/env {:foo 100}
      :monad.state/state {:blah :blah}}))

  (def ce
    (m.prws/run-prws
     (m.prws/prws-let
      [a (e/catch ex-data emv)]
      (m/return a))
     {:monad.reader/env {:foo 10}
      :monad.state/state {:blah :blah}}))
  )
