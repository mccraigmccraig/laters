(ns laters.control.exception
  (:require
   [laters.abstract.context.protocols :as ctx.p]
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.error.protocols :as err.p]
   [laters.abstract.tagged :as tagged]
   [laters.control.identity :as id]))

(defrecord Success [ctx v]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] v))

(defn success [ctx v]
  (->Success ctx v))

(defn success?
  [v]
  (instance? Success v))

(defrecord Failure [ctx e]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] e))

(defn failure [ctx e]
  (->Failure ctx e))

(defn failure?
  [v]
  (instance? Failure v))

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
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-type [m])
  (-bind [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [mv]
       (try
         (cond
           (success? mv) (f (ctx.p/-extract mv))
           (failure? mv) (m.p/-return inner-ctx mv)
           :else (m.p/-return
                  inner-ctx
                  (failure
                   (ex-info "illegal mv" {:mv mv}))))
         (catch Exception e
           (m.p/-return inner-ctx (failure m e)))))))
  (-return [m v]
    (m.p/-return inner-ctx (success m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (failure m v)))
  (-catch [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [mv]
       (if (failure? mv)
         (try
           (f (ctx.p/-extract mv))
           (catch Exception e
             (m.p/-return inner-ctx (failure m e))))
         (m.p/-return inner-ctx mv)))))
  (-finally [m mv f]
    mv))

(def exception-ctx
  (->ExceptionTCtx (id/->IdentityCtx [::ExceptionT ::id/Identity])))

(def tagged-exception-ctx
  (->ExceptionTCtx (tagged/->TaggedCtx [::ExceptionT ::tagged/Tagged] nil)))
