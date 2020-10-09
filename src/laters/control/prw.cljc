(ns laters.control.prw
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.abstract.error.protocols :as e.p]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.promise :as m.pr]
   [laters.concurrency.promise :as p]
   [laters.concurrency.promise.promesa :as promesa])
  (:import
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

;; ({:monad.reader/env r})->Promise<{:monad/val v :monad.writer/output w}
(deftype PRW [promise-impl lifter]
  m.p/Monad
  (-type [m]
    (into [::PRW] (p/type promise-impl)))
(-bind [m mv f]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/handle
        (p/pcatch promise-impl ((l/lift-untag lifter m mv) {:monad.reader/env env}))
        (fn [{w :monad.writer/output
             v :monad/val
             :as success}
            error]
          (if (some? error)
            (p/rejected promise-impl (concat-error error []))
            (p/handle
             (p/pcatch promise-impl ((l/lift-untag lifter m (f v)) {:monad.reader/env env}))
             (fn [{w' :monad.writer/output
                  v' :monad/val :as success}
                 error]
               (if (some? error)
                 (p/rejected promise-impl (concat-error error w))
                 (p/resolved
                  promise-impl
                  {:monad.writer/output ((fnil into []) w w')
                   :monad/val v'})))
             promise-impl)))
        promise-impl))))
  (-return [m v]
    (t/tag
     m
     (fn [_]
       (p/resolved
        promise-impl
        {:monad.writer/output []
         :monad/val v}))))
  e.p/MonadError
  (-reject [m v]
    (t/tag
     m
     (fn [_]
       (p/rejected
        promise-impl
        (prw-error v)))))

  ;; catches an errored PRW mv
  ;;  - outputs any captured output
  ;;  - calls (handler error) to generate a new mv
  ;;  - calls that with the environment
  ;;  - returns new output and value
  (-catch [m mv handler]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/handle
        (p/pcatch promise-impl ((l/lift-untag lifter m mv) {:monad.reader/env env}))
        (fn [success error]
          (if (some? error)

            (let [error (unwrap-error error)
                  {error-output-a :monad.writer/output
                   cause :monad/error} (ex-data error)
                  error-output (some-> error-output-a deref)

                  r-mv (handler cause)]

              (p/handle
               (p/pcatch promise-impl
                         ((l/lift-untag lifter m r-mv)
                          {:monad.reader/env env}))

               (fn [{r-w :monad.writer/output
                    r-v :monad/val
                    :as r-success}
                   r-error]
                 (if (some? r-error)
                   (p/rejected promise-impl concat-error r-error error-output)
                   (p/resolved
                    promise-impl
                    {:monad.writer/output ((fnil into []) error-output r-w)
                     :monad/val r-v})))))

            success))))))
  m.p/MonadZero
  (-mzero [m]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/rejected
        promise-impl
        (ex-info
         ":mopr.control.monad/mzero"
         {:monad.writer/output [::mzero]
          :monad/val nil})))))
  m.r/MonadReader
  (-ask [m]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        promise-impl
        {:monad.writer/output nil
         :monad/val env}))))
  (-local [m f mv]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       ((l/lift-untag lifter m mv)
        {:monad.reader/env (f env)}))))

  m.w/MonadWriter
  (-tell [m v]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        promise-impl
        {:monad.writer/output [v] :monad/val nil}))))
  (-listen [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/then
        ((l/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             v :monad/val
             :as lv}]
          {:monad.writer/output w
           :monad/val lv})
        promise-impl))))
  (-pass [m mv]
    (t/tag
     m
     (fn [{env :monad.reader/env}]
       (p/then
        ((l/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             pass-val :monad/val}]
          (let [[val f] (m.w/-as-vec pass-val)]
            {:monad.writer/output (f w)
             :monad/val val}))
        promise-impl)))))

(defn prw-lifters
  [to-promise-impl]
  {[::m.id/Identity] (fn [mv]
                       (fn [{r :monad.reader/env}]
                         (p/resolved
                          to-promise-impl
                          {:monad.writer/output nil :monad/val mv})))

   ;; lift any recognised type of plain promise!
   [::m.pr/Promise :type/*] (fn [mv]
                              (fn [{r :monad.reader/env}]
                                (let [d (p/deferred to-promise-impl)]
                                  (p/handle
                                   mv
                                   (fn [v error]
                                     (if (nil? error)
                                       (p/resolve!
                                        d
                                        {:monad.writer/output nil
                                         :monad/val v})
                                       (p/reject!
                                        (prw-error error))))
                                   to-promise-impl))))})

(defn make-prw-ctx
  [promise-impl lifter]
  (PRW. promise-impl lifter))

(def prw-lifter (l/create-atomic-lifter-registry))

(def prw-ctx (make-prw-ctx promesa/default-impl prw-lifter))

(l/register-all prw-lifter prw-ctx (prw-lifters promesa/default-impl))

(m/deflets
  {prw-let laters.control.prw/prw-ctx})

(defn run-prw
  [mv rw]
  ((t/untag mv) rw))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.error :as e])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.maybe :as m.maybe])
  (require '[laters.control.reader :as m.reader])
  (require '[laters.control.writer :as m.writer])
  (require '[laters.control.promise :as m.pr])
  (require '[laters.control.prw :as m.prw])
  (require '[laters.concurrency.promise :as p])
  (require '[laters.concurrency.promise.rxjava :as rxjava])

        @(m.prw/run-prw
    (m.prw/prw-let
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
      (m.prw/prw-let
       [a (m.reader/asks :foo)
        _ (m.writer/tell [:foo a])
        b (m/return 5)
        _ (m.writer/tell :bar)
        _ (throw (ex-info "wah" {:a a :b b}))]
       (m/return [a b])))

  (def er
    (m.prw/run-prw
     emv
     {:monad.reader/env {:foo 100}}))

  (def ce
    (m.prw/run-prw
     (m.prw/prw-let
      (e/catch emv #(m/return (ex-data %))))
     {:monad.reader/env {:foo 10}}))

  ;; catch in first step
  @(m.prw/run-prw
    (m.prw/prw-let
     (e/catch
         (throw (ex-info "boo" {:foo 200}))
         (fn [e]
           (m.prw/prw-let
            [_ (m.writer/tell [:error (.getMessage e)])]
            (m/return (ex-data e))))))
    {})

  ;; RxJava Single-based promises
  (def prw-ctx
    (m.prw/make-prw-ctx
     rxjava/factory {}))

  ;; does some stuff, then errors
  (def emv
    (m/mlet prw-ctx
      [a (m.reader/asks :foo)
       _ (m.writer/tell [:foo a])
       b (m/return 5)
       _ (m.writer/tell :bar)
       _ (throw (ex-info "wah" {:a a :b b}))]
      (m/return [a b])))

  ;; catches any error, retrieving writer log
  (def ce
    (m.prw/run-prw
     (m/mlet prw-ctx
       (e/catch
           emv
           (fn [e]
             (m/mlet prw-ctx
               [_ (m.writer/tell [:error (.getMessage e)])]
               (m/return (ex-data e))))))
     {:monad.reader/env {:foo 10}}))

  (p/deref ce)
  )
