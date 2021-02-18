(ns laters.control.identity
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.context.protocols :as ctx.p]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Identity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype IdentityCtx [t]
  ctx.p/Context
  (-get-type [m] t)
  m.p/Monad
  (-bind [m mv f]
    (f mv))
  (-join [m mmv]
    mmv)
  (-return [m v]
    v))

(def identity-ctx (->IdentityCtx [::Identity]))
