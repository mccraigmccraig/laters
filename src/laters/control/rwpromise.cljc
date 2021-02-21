(ns laters.control.rwpromise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.context.protocols :as ctx.p]
   [laters.abstract.monad :as m]
   [laters.abstract.error.protocols :as err.p]
   [laters.control.reader.protocols :as m.r.p]
   [laters.control.writer.protocols :as m.w.p]
   [laters.control.identity :as id]
   [laters.abstract.tagged :as tagged]
   [laters.abstract.runnable.protocols :as runnable.p]
   [laters.monoid :as monoid]
   [promesa.core :as p])
  (:import
   [java.util.concurrent
    ExecutionException
    CompletionException]))

(def exception-wrapper-classes
  #{ExecutionException CompletionException})

(defn unwrap-exception
  [e]
  (if (contains?
       exception-wrapper-classes
       (some-> e .getClass))
    (.getCause e)
    e))

;; values are: <env> -> Promise<writer,val>
(defrecord RWPromiseVal [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn rwpromise-val
  [ctx f]
  (->RWPromiseVal ctx f))

(defn rwpromise-val?
  [v]
  (instance? RWPromiseVal v))

(defn plain-rwpromise-val
  [ctx v]
  (rwpromise-val
   ctx
   (fn [_]
     (p/resolved
      {:monad.writer/output nil
       :monad/val v}))))

(defn error-rwpromise-body
  [output-ctx e w]
  (let [d (ex-data e)]
    (if (contains? d :monad/error-channel)
      (do
        (swap! (get-in d [:monad/error-channel
                          :monad.writer/output])
               #(monoid/mappend output-ctx % w))
        (p/rejected e))

      (p/rejected
       (ex-info
        "RWPromise error"
        {:monad/error-channel
         {:monad.writer/output (monoid/mappend output-ctx nil w)}
         :cause/data d}
        e)))))

(defn error-rwpromise-val
  [ctx output-ctx e w]
  (rwpromise-val
   ctx
   (fn [_]
     (error-rwpromise-body output-ctx e w))))

(defn rw-promise-t-bind-2
  ([output-ctx inner-ctx m inner-mv discard-val? inner-2-mf]
   (m.p/-bind
    inner-ctx
    inner-mv

    (fn outer-mf [outer-mv]
      (assert (rwpromise-val? outer-mv))

      (m.p/-return
       inner-ctx
       (rwpromise-val
        m
        (fn [{env :monad.reader/env}]

          (p/handle
           (try
             (runnable.p/-run outer-mv {:monad.reader/env env})
             (catch Exception e
               ;; (prn "catching 1" e)
               (error-rwpromise-body output-ctx e nil)))
           (fn [{w :monad.writer/output
                v :monad/val
                :as right}
               left]
             ;; (prn "left-right" [left right])
             (let [inner-mv' (try
                               (inner-2-mf (some-> left unwrap-exception) v)
                               (catch Exception e
                                 ;; (prn "catching 2" e)
                                 (error-rwpromise-val inner-ctx output-ctx e w)))]

               (m.p/-bind
                inner-ctx
                inner-mv'

                (fn outer-mf' [outer-mv']
                  (assert (rwpromise-val? outer-mv'))

                  (p/handle
                   (try
                     (runnable.p/-run outer-mv' {:monad.reader/env env})
                     (catch Exception e
                       ;; (prn "catching 3" e)
                       (error-rwpromise-body output-ctx e nil)))
                   (fn [{w' :monad.writer/output
                        v' :monad/val
                        :as right}
                       left]

                     ;; (prn "left-right 2" [left right])

                     (if (some? left)
                       ;; forward errors on
                       (throw (unwrap-exception left))

                       {:monad.writer/output (monoid/mappend
                                              output-ctx
                                              nil
                                              w
                                              w')
                        :monad/val (if discard-val? v v')})))))))))))))))

(deftype RWPromiseTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (rw-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       (prn left right)
       (if (some? left)
         (err.p/-reject m left)
         (inner-mf right)))))

  (-return [m v]
    (m.p/-return inner-ctx (plain-rwpromise-val m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (error-rwpromise-val m v)))
  (-handle [m inner-mv inner-mf2]
    (rw-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     inner-mf2))
  (-catch [m inner-mv inner-mf]
    (rw-promise-t-bind-2
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
    (rw-promise-t-bind-2
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
     (rwpromise-val
      m
      (fn [{env :monad.reader/env}]
        {:monad.writer/output nil
         :monad/val env}))))
  (-local [m f mv]
    (m.p/-return
     inner-ctx
     (rwpromise-val
      m
      (fn [{env :monad.reader/env}]
        (runnable.p/-run mv {:monad.reader/env (f env)})))))

  m.w.p/MonadWriter
  (-tell [m v]
    (m.p/-return
     inner-ctx
     (rwpromise-val
      m
      (fn [{env :monad.reader/env}]
        {:monad.writer/output (monoid/mappend output-ctx nil v)}))))
  (-listen [m mv]
    (m.p/-return
     inner-ctx
     (rwpromise-val
      m
      (fn [{env :monad.reader/env}]
        (let [{w :monad.writer/output
               :as lv} (runnable.p/-run mv {:monad.reader/env env})]
          {:monad.writer/output w
           :monad/val lv})))))
  (-pass [m mv]
    (m.p/-return
     inner-ctx
     (rwpromise-val
      m
      (fn [{env :monad.reader/env}]
        (let [{w :monad.writer/output
               [val f] :monad/val} (runnable.p/-run mv {:monad.reader/env env})]
          {:monad.writer/output (f w)
           :monad/val val}))))))

(def rwpromise-ctx
  (->RWPromiseTCtx
   monoid/map-monoid-ctx
   (id/->IdentityCtx [::RWPromiseT ::monoid.map ::id/identity])))

(def tagged-rwpromise-ctx
  (->RWPromiseTCtx
   monoid/map-monoid-ctx
   (tagged/->TaggedCtx [::RWPromiseT ::monoid.map ::tagged/Tagged] nil)))


(comment
  (require '[laters.control.rwpromise :as rwp])
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.reader :as reader])
  (require '[laters.control.writer :as writer])
  (require '[laters.abstract.error :as e])
  (require '[laters.abstract.runnable :as r])


  (def mv2 (m/bind
            rwp/rwpromise-ctx
            (m/return rwp/rwpromise-ctx 10)
            (fn [a] (m/return
                    rwp/rwpromise-ctx
                    (inc a)))))

  @(r/run mv2 {:monad.reader/env 10})

  )
