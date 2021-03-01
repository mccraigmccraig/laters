(ns promisefx.control.exception
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.fx.error.protocols :as err.p]
   [promisefx.data.extractable.protocols :as extractable.p]
   [promisefx.data.success-failure :as s.f]
   [promisefx.control.identity :as ctrl.id]
   [promisefx.control.tagged :as ctrl.tag]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ExceptionTCtx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; values are Success|Failure
;; Failures short-circuit
;; Exceptions are wrapped in a Failure
;; anything can be put in a Failure with error/reject
;; Failures can be caught with error/catch
;; finally behaviour with error/finally

;; using the transformer around a "Tagged" context gives auto-lifting of mvs

(deftype ExceptionTCtx [inner-ctx]
  ctx.p/Context
  (-get-tag [m] (ctx.p/-get-tag inner-ctx))
  m.p/Monad
  (-bind [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [mv]
       (try
         (cond
           (s.f/success? mv) (f (extractable.p/-extract mv))
           (s.f/failure? mv) (m.p/-return inner-ctx mv)
           :else (m.p/-return
                  inner-ctx
                  (s.f/failure
                   (ex-info "illegal mv" {:mv mv}))))
         (catch Exception e
           (m.p/-return inner-ctx (s.f/failure m e)))))))
  (-return [m v]
    (m.p/-return inner-ctx (s.f/success m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (s.f/failure m v)))
  (-catch [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [mv]
       (if (s.f/failure? mv)
         (try
           (f (extractable.p/-extract mv))
           (catch Exception e
             (m.p/-return inner-ctx (s.f/failure m e))))
         (m.p/-return inner-ctx mv)))))
  (-finally [m mv f]
    mv))

(def ctx
  (->ExceptionTCtx (ctrl.id/->IdentityCtx [::ExceptionT ::ctrl.id/Identity])))

(def tagged-ctx
  (->ExceptionTCtx (ctrl.tag/->TaggedCtx [::ExceptionT ::ctrl.tag/Tagged] nil)))
