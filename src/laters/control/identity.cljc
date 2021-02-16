(ns laters.control.identity
  (:require
   [laters.abstract.monad.protocols :as m.p]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Identity context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype IdentityCtx []
  m.p/Monad
  (-type [m]
    [::Identity])
  (-bind [m mv f]
    (f mv))
  (-join [m mmv]
    mmv)
  (-return [m v]
    v))

(def identity-ctx (->IdentityCtx))
