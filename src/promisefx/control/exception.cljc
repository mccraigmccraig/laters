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
;;; ExceptionCtx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; values are Failure|anything-else
;; Failures short-circuit
;; thrown Exceptions get caught and wrapped in a Failure
;; anything can be put in a Failure with error/reject
;; Failures can be caught with error/catch
;; finally behaviour with error/finally

(deftype ExceptionTCtx [tag inner-ctx]
  ctx.p/Context
  (-get-tag [m] tag)

  m.p/Monad
  (-bind [m mv f]
    (try
      (if (s.f/failure? mv)
        mv
        (f mv))
      (catch #?(:clj Exception :cljs :default) e
        (s.f/failure m e))))
  (-return [m v]
    v)

  err.p/MonadError
  (-reject [m v]
    (s.f/failure m v))
  (-catch [m mv f]
    (if (s.f/failure? mv)
      (try
        (f (extractable.p/-extract mv))
        (catch #?(:clj Exception :cljs :default) e
          (s.f/failure m e)))
      mv))
  (-finally [m mv f]
    mv))

(def untagged-ctx
  (->ExceptionTCtx
   [::ExceptionT ::Identity]
   ctrl.id/ctx))

(def ctx
  (->ExceptionTCtx
   [::ExceptionT ::Tagged]
   (ctrl.tag/->TaggedCtx [::ExceptionT ::Tagged] nil)))
