(ns promisefx.control.identity
  (:require
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.context.protocols :as ctx.p]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Identity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype IdentityCtx []
  ctx.p/Context
  (-get-tag [m] [::Identity])
  m.p/Monad
  (-bind [m mv f]
    (f mv))
  (-join [m mmv]
    mmv)
  (-return [m v]
    v))

(def ctx (->IdentityCtx))
