(ns laters.control.rwexception
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.context.protocols :as ctx.p]
   [laters.abstract.monad :as m]
   [laters.abstract.error.protocols :as err.p]
   [laters.abstract.error :as error]
   [laters.control.reader.protocols :as m.r.p]
   [laters.control.writer.protocols :as m.w.p]
   [laters.control.maybe :as maybe]
   [laters.control.exception :as exception]
   [laters.control.identity :as id]
   [laters.abstract.tagged :as tagged]
   [laters.abstract.runnable.protocols :as runnable.p]
   [laters.monoid :as monoid]))

(defrecord RWExceptionVal [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn rwexception-val
  [ctx f]
  (->RWExceptionVal ctx f))

(defn rwexception-val?
  [v]
  (instance? RWExceptionVal v))

(defn plain-rwexception-val
  [ctx v]
  (rwexception-val
   ctx
   (fn [_]
     {:monad.writer/output nil
      :monad/val v})))

(defn error-rw-exception-body
  ([e] (error-rw-exception-body nil e))
  ([output e]
   (merge
    {:monad.writer/output nil}
    output
    { :monad/val (exception/failure e)})))

(defn error-rwexception-val
  ([ctx e] (error-rwexception-val ctx nil e))
  ([ctx output e]
   (rwexception-val
    ctx
    (fn [_]
      (error-rw-exception-body output e)))))

(deftype RWExceptionTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (m.p/-bind
     inner-ctx
     inner-mv

     (fn outer-mf [outer-mv]
       (assert (rwexception-val? outer-mv))

       (m.p/-return
        inner-ctx
        (rwexception-val
         m
         (fn [{env :monad.reader/env}]

           (try
             (let [{w :monad.writer/output
                    v :monad/val
                    :as r} (runnable.p/-run outer-mv {:monad.reader/env env})]

               (if (exception/failure? v)

                 r

                 (let [inner-mv' (inner-mf v)]

                   (m.p/-bind
                    inner-ctx
                    inner-mv'

                    (fn outer-mf' [outer-mv']
                      (assert (rwexception-val? outer-mv'))

                      (try
                        (let [{w' :monad.writer/output
                               v' :monad/val} (runnable.p/-run
                                               outer-mv'
                                               {:monad.reader/env env})]
                          {:monad.writer/output (monoid/mappend
                                                 output-ctx
                                                 nil
                                                 w
                                                 w')
                           :monad/val v'})
                        (catch Exception e
                          (error-rw-exception-body
                           {:monad.writer/output (monoid/mappend
                                                  output-ctx
                                                  nil
                                                  w)}
                           e))))))))
             (catch Exception e
               (error-rw-exception-body e)))))))))
  (-return [m v]
    (m.p/-return
     inner-ctx
     (plain-rwexception-val m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx ( v)))
  (-catch [m mv f])
  (-finally [m mv f])

  m.r.p/MonadReader
  (-ask [m])
  (-local [m f mv])

  m.w.p/MonadWriter
  (-tell [m v])
  (-listen [m mv])
  (-pass [m mv]))

(def rwexception-ctx
  (->RWExceptionTCtx
   monoid/map-monoid-ctx
   (id/->IdentityCtx [::RWExceptionT ::monoid/map ::id/Identity])))

(def tagged-rwexception-ctx
  (->RWExceptionTCtx
   monoid/map-monoid-ctx
   (tagged/->TaggedCtx [::RWExceptionT ::monoid/map ::tagged/Tagged] nil)))
