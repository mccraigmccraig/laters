(ns laters.control.err
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.error.protocols :as err.p]
   [laters.abstract.error :as err]
   [laters.abstract.tagged :as tag]
   [laters.control.identity :as id]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ErrT context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ErrTCtx [inner-ctx]
  m.p/Monad
  (-type [m])
  (-bind [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [err-v]
       (if (err/error? err-v)
         err-v
         (try
           (f err-v)
           (catch Exception e
             (m.p/-return
              inner-ctx
              (err/error-marker e))))))))
  (-return [m v]
    (m.p/-return
     inner-ctx
     v))
  m.p/MonadZero
  (-mzero [m]
    (m.p/-return
     inner-ctx
     (err/error-marker nil)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return
     inner-ctx
     (err/error-marker v)))
  (-catch [m mv f]
    (m.p/-bind
     inner-ctx
     mv
     (fn [err-v]
       (if (err/error? err-v)
         (try
           (f err-v)
           (catch Exception e
             (m.p/-return
              inner-ctx
              (err/error-marker e))))
         (m.p/-return
          inner-ctx
          err-v)))))
  (-finally [m mv f]
    mv))

(def err-ctx
  (->ErrTCtx id/identity-ctx))

(def tagged-err-ctx
  (->ErrTCtx (tag/->TaggedCtx ::Err nil)))
