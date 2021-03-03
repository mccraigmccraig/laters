(ns promisefx.control.rws-promise
  (:require
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.context.protocols :as ctx.p]
   [promisefx.fx.monad :as m]
   [promisefx.fx.error.protocols :as err.p]
   [promisefx.fx.reader.protocols :as m.r.p]
   [promisefx.fx.writer.protocols :as m.w.p]
   [promisefx.control.identity :as ctrl.id]
   [promisefx.control.tagged :as ctrl.tagged]
   [promisefx.data.extractable.protocols :as extractable.p]
   [promisefx.data.runnable.protocols :as runnable.p]
   [promisefx.data.monoid :as monoid]
   [promesa.core :as p])
  #?(:clj
     (:import
      [java.util.concurrent
       ExecutionException
       CompletionException])))

(def ^:private exception-wrapper-classes
  #?(:clj #{ExecutionException CompletionException}
     :cljs #{}))

(defn platform-exception-wrapper?
  [e]
  #?(:clj (contains? exception-wrapper-classes (some-> e .getClass))
     :cljs false))

(defn unwrap-exception
  "remove j.u.c exception wrappers"
  [e]
  (if (platform-exception-wrapper? e)
    (ex-cause e)
    e))

;; values are: <env,state> -> Promise<log,state,val>
(defrecord RWSPromiseMV [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn rws-promise-mv
  [ctx f]
  (->RWSPromiseMV ctx f))

(defn rws-promise-mv?
  [v]
  (instance? RWSPromiseMV v))

(defn success-rws-promise-mv
  [ctx v]
  (rws-promise-mv
   ctx
   (fn [_]
     (p/resolved
      {:promisefx.writer/output nil
       :promisefx/val v}))))

(defn unwrap-failure-channel
  "we use an ex-data to propagate effects in case of failure. we do
   this because j.u.c.CompletableFutures only supports Exceptions as
   failure cases"
  [e]
  ;; (prn "unwrap-failure-channel" (unwrap-exception e))
  (some-> e
          unwrap-exception
          ex-data
          :promisefx.rwpromise/failure-channel
          ;; a failure-channel should never include a value. remove
          ;; it to prevent any confusion (e.g. left + right both some?)
          (dissoc :promisefx/val)))

(defn unwrap-cause
  "get at the original exception

  if the provided exception has failure-channel data, then it wraps the
  original exception

  if the provided exception has no failure-channel data, then it is the
  original exception"
  [e]
  (let [e (unwrap-exception e)
        has-failure-channel? (some? (some-> e
                                            ex-data
                                            :promisefx.rwpromise/failure-channel))]
    (if has-failure-channel?
      (ex-cause e)
      e)))

(defn failure-rws-promise-result
  "take an exception and some failure-channel data, and return
   a Promise of failure... which is an ex-info holding the failure
   channel data

   if the provided exception already has failure-channel data, then
   it's a wrapped exception, in which case, preserve the original cause
   in a new wrapper (along with the provided failure-channel data)

   if the provided exception does not have failure-channel data, then
   it is the cause, so use it as the cause in a new wrapper, along with
   the provided failure-channel data

   t: Promise<log,val>"
  ([e] (failure-rws-promise-result e {:promisefx.writer/output nil}))

  ([e failure-channel]
   (let [e (unwrap-exception e)
         has-failure-channel? (some? (some-> e
                                             ex-data
                                             :promisefx.rwpromise/failure-channel))
         ;; preserve original cause if we are given a wrapped exception
         cause (if has-failure-channel?
                 (ex-cause e)
                 e)]

     ;; (prn "failure-rws-promise-result" e cause failure-channel)

     (p/rejected
      (ex-info
       "RWPromise failure"
       {:promisefx.rwpromise/failure-channel failure-channel
        :cause/data (ex-data cause)}
       cause)))))

(defn failure-rws-promise-mv
  "t: <env> -> Promise<log,val>"
  [ctx e]
  (rws-promise-mv
   ctx
   (fn [_]
     (failure-rws-promise-result e))))

(defn rws-promise-t-bind-2
  ([output-ctx inner-ctx m inner-mv discard-val? inner-2-mf]
   (m.p/-bind
    inner-ctx
    inner-mv

    (fn outer-mf [outer-mv]
      (assert (rws-promise-mv? outer-mv))

      (m.p/-return
       inner-ctx
       (rws-promise-mv
        m
        (fn [{env :promisefx.reader/env}]

          (p/handle
           (try
             (runnable.p/-run outer-mv {:promisefx.reader/env env})
             (catch #?(:clj Exception :cljs :default) e
               ;; (prn "catching 1" e)
               (failure-rws-promise-result e)))
           (fn [right left]
             ;; (prn "handle1" [right left])

             (let [left? (some? left)
                   {w :promisefx.writer/output
                    v :promisefx/val} (if left?
                                    (unwrap-failure-channel left)
                                    right)
                   ;; _ (prn "handle1-unwrapped" [w v])

                   inner-mv' (try
                               (inner-2-mf
                                (when left? (some-> left unwrap-cause))
                                (when-not left? v))
                               (catch #?(:clj Exception :cljs :default) e
                                 ;; (prn "catching 2" e)
                                 (failure-rws-promise-mv inner-ctx e)))]

               (m.p/-bind
                inner-ctx
                inner-mv'

                (fn outer-mf' [outer-mv']
                  (assert (rws-promise-mv? outer-mv'))

                  (p/handle
                   (try
                     (runnable.p/-run outer-mv' {:promisefx.reader/env env})
                     (catch #?(:clj Exception :cljs :default) e
                       ;; (prn "catching 3" e)
                       (failure-rws-promise-result e)))

                   (fn [right' left']
                     ;; (prn "handle2" [right' left'])

                     (let [left'? (some? left')
                           {w' :promisefx.writer/output
                            v' :promisefx/val} (if left'?
                                             (unwrap-failure-channel left')
                                             right')

                           w'' (monoid/mappend output-ctx w w')]
                       ;; _ (prn "handle2-unwrapped" [w' v' w''])

                       ;; (prn "left-right 2" [left right])

                       (if left'?
                         (failure-rws-promise-result
                          left'
                          {:promisefx.writer/output w''})

                         {:promisefx.writer/output w''
                          :promisefx/val (if discard-val? v v')}))))))))))))))))

(deftype RWSPromiseTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-tag [m] (ctx.p/-get-tag inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (rws-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       ;; (prn left right)
       (if (some? left)
         (err.p/-reject m left)
         (inner-mf right)))))

  (-return [m v]
    (m.p/-return inner-ctx (success-rws-promise-mv m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (failure-rws-promise-mv m v)))
  (-handle [m inner-mv inner-mf2]
    (rws-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     inner-mf2))
  (-catch [m inner-mv inner-mf]
    (rws-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       ;; (prn "catch" [left right])
       (if (some? left)
         (inner-mf left)
         (m.p/-return m right)))))
  (-finally [m inner-mv inner-mf]
    (rws-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     true
     (fn [_left _right]
       (inner-mf))))

  m.r.p/MonadReader
  (-ask [m]
    (m.p/-return
     inner-ctx
     (rws-promise-mv
      m
      (fn [{env :promisefx.reader/env}]
        {:promisefx.writer/output nil
         :promisefx/val env}))))
  (-local [m f mv]
    (m.p/-return
     inner-ctx
     (rws-promise-mv
      m
      (fn [{env :promisefx.reader/env}]
        (runnable.p/-run mv {:promisefx.reader/env (f env)})))))

  m.w.p/MonadWriter
  (-tell [m v]
    (m.p/-return
     inner-ctx
     (rws-promise-mv
      m
      (fn [{_env :promisefx.reader/env}]
        {:promisefx.writer/output (monoid/mappend output-ctx nil v)}))))
  (-listen [m mv]
    (m.p/-return
     inner-ctx
     (rws-promise-mv
      m
      (fn [{env :promisefx.reader/env}]
        (let [{w :promisefx.writer/output
               :as lv} (runnable.p/-run mv {:promisefx.reader/env env})]
          {:promisefx.writer/output w
           :promisefx/val lv})))))
  (-pass [m mv]
    (m.p/-return
     inner-ctx
     (rws-promise-mv
      m
      (fn [{env :promisefx.reader/env}]
        (let [{w :promisefx.writer/output
               [val f] :promisefx/val} (runnable.p/-run mv {:promisefx.reader/env env})]
          {:promisefx.writer/output (f w)
           :promisefx/val val}))))))

(def ctx
  (->RWSPromiseTCtx
   monoid/map-monoid-ctx
   (ctrl.id/->IdentityCtx [::RWPromiseT ::monoid.map ::ctrl.id/identityCtx])))

(def tagged-ctx
  (->RWSPromiseTCtx
   monoid/map-monoid-ctx
   (ctrl.tagged/->TaggedCtx [::RWPromiseT ::monoid.map ::ctrl.tagged/TaggedCtx] nil)))


(comment
  (require '[promisefx.control.rwpromise :as rwp])
  (require '[promisefx.abstract.monad :as m])
  (require '[promisefx.control.reader :as reader])
  (require '[promisefx.control.writer :as writer])
  (require '[promisefx.abstract.error :as e])
  (require '[promisefx.abstract.runnable :as r])


  (def mv2 (m/bind
            rwp/rwpromise-ctx
            (m/return rwp/rwpromise-ctx 10)
            (fn [a] (m/return
                    rwp/rwpromise-ctx
                    (inc a)))))

  @(r/run mv2 {:promisefx.reader/env 10})

  )
