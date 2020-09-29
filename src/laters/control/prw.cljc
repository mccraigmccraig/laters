(ns laters.control.prw
  (:require
   [clojure.string :as str]
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
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

(defn is-prw-error?
  [error]
  (and (instance? ExceptionInfo error)
       (= ::error (-> error ex-data :type))
       (instance? Atom (-> error ex-data :monad.writer/output))))

(defn prw-error
  ([error] (prw-error error nil))
  ([error prior-output]
   (ex-info
    (str ::error)
    {:type ::error
     :monad/error error
     :monad.writer/output (atom prior-output)})))

(defn concat-error
  [error prior-output]
  (let [error (unwrap-error error)]
    (if (is-prw-error? error)

      (do
        (swap!
         (-> error ex-data :monad.writer/output)
         (fn [output]
           ((fnil into []) prior-output output)))

        error)

      (prw-error error prior-output))))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [& body]
  `(try
     ~@body
     (catch Exception x#
       (p/rejected x#))))

;; ({:monad.reader/env r})->Promise<{:monad/val v :monad.writer/output w}
(deftype PRW [lifter]
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (-> (pcatch ((m/lift-untag lifter m wmv) {:monad.reader/env env}))
           (p/handle
            (fn [{w :monad.writer/output
                 v :monad/val :as success}
                error]
              (if (some? error)
                (p/rejected (concat-error error []))
                (p/handle
                 (pcatch ((m/lift-untag lifter m (f v)) {:monad.reader/env env}))
                 (fn [{w' :monad.writer/output
                      v' :monad/val :as success}
                     error]
                   (if (some? error)
                     (p/rejected (concat-error error w))
                     (p/resolved
                      {:monad.writer/output ((fnil into []) w w')
                       :monad/val v'})))))))))))
  (-return [m v]
    (m/tag
     m
     (fn [_]
       (p/resolved
        {:monad.writer/output []
         :monad/val v}))))
  m.pr/MonadPromise
  (-reject [m v]
    (m/tag
     m
     (fn [_]
       (p/rejected
        (prw-error v)))))

  ;; catches an errored PRW mv
  ;;  - outputs any captured output
  ;;  - returns a val with (handler original-error)
  (-catch [m handler mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (-> (pcatch ((m/lift-untag lifter m mv) {:monad.reader/env env}))
           (p/handle
            (fn [success error]
              (if (some? error)

                (let [error (unwrap-error error)
                      {output-a :monad.writer/output
                       cause :monad/error} (ex-data error)

                      output (some-> output-a deref)
                      val (handler cause)]

                  {:monad.writer/output output
                   :monad/val val})

                success)))))))
  m/MonadZero
  (-mzero [m]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/rejected
        (ex-info
         ":mopr.control.monad/mzero"
         {:monad.writer/output [::mzero]
          :monad/val nil})))))
  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        {:monad.writer/output nil
         :monad/val env}))))
  (-local [m f mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       ((m/lift-untag lifter m mv)
        {:monad.reader/env (f env)}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        {:monad.writer/output [v] :monad/val nil}))))
  (-listen [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             v :monad/val
             :as lv}]
          {:monad.writer/output w
           :monad/val lv})))))
  (-pass [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             pass-val :monad/val}]
          (let [[val f] (m.w/-as-vec pass-val)]
            {:monad.writer/output (f w)
             :monad/val val})))))))

(def prw-lifters
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :monad.reader/env}]
                         (p/resolved
                          {:monad.writer/output nil :monad/val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :monad.reader/env}]
                        (p/chain
                         mv
                         (fn [v]
                           {:monad.writer/output nil :monad/val v}))))})

(def prw-lifter (m/create-atomic-lifter))

(def prw-ctx (PRW. prw-lifter))

(doseq [[from-ctx lifter] prw-lifters]
  (m/-register prw-lifter prw-ctx from-ctx lifter))

(m/deflets
  {prw-let prw-ctx})

(defn run-prw
  [wmv rw]
  ((m/untag wmv) rw))


(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.reader :as m.reader])
  (require '[laters.control.writer :as m.writer])
  (require '[laters.control.promise :as m.pr])
  (require '[laters.control.prw :as m.prw])

  @(m.prw/run-prw
    (m/mlet m.prw/prw-ctx
      [{a :foo} (m.reader/ask)
       b (m.reader/asks :bar)
       c (m/return (+ a b))
       _ (m.writer/tell c)
       d (m.reader/local
          #(assoc % :foo 20)
          (m/mlet m.prw/prw-ctx
            [{a :foo b :bar} (m.reader/ask)]
            (m/return (+ a b))))
       {listen-out :monad.writer/output
        e :monad/val} (m.writer/listen
                       (m/mlet m.prw/prw-ctx
                         [_ (m.writer/tell :foofoo)
                          _ (m.writer/tell :barbar)]
                         (m/return :blah)))
       _ (m.writer/tell listen-out)
       f (m.writer/pass
          (m/mlet m.prw/prw-ctx
            [_ (m.writer/tell :one)
             _ (m.writer/tell :two)
             _ (m.writer/tell :one)]
            (m/return [:passed (fn [out] (filter #(= :one %) out))])))
       g (m/mlet m.pr/promise-ctx
           [a (m/return 100)
            b (m/return 100)]
           (m/return (* a a)))]
      (m/return [a b c d e f g]))
    {:monad.reader/env {:foo 10 :bar 20}})

  ;; catch

  (def emv
    (m/mlet m.prw/prw-ctx
      [a (m.reader/asks :foo)
       _ (m.writer/tell [:foo a])
       b (m/return 5)
       _ (m.writer/tell :bar)
       _ (throw (ex-info "wah" {:a a :b b}))]
      (m/return [a b])))

  (def er
    (m.prw/run-prw
     emv
     {:monad.reader/env {}}))

  (def ce
    (m.prw/run-prw
     (m/mlet m.prw/prw-ctx
       [a (m.pr/catch ex-data emv)]
       (m/return a))
     {:monad.reader/env {:foo 10}})))
