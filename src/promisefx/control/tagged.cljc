(ns promisefx.control.tagged
  (:require
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.context.protocols :as ctx.p]
   [promisefx.context.lifter :as lifter]
   [promisefx.data.tagged.protocols :as tagged.p]))

;; a Tagged context, for use as an inner-context with
;; monad transformes to perform auto-lifting of values
;; IdentityTransformer ... as long as types match,
;; it's an IdentityCtx, if types don't match
;; it attempts to lift
;;
;; ErrorTransformer
;; RWErrorTransformer
;; PRWTransformer
;; etc

(deftype TaggedCtx [t lifter]
  ctx.p/Context
  (-get-tag [m] t)

  m.p/Monad
  (-bind [m mv f]
    (let [mv' (f mv)
          mv'-tag (tagged.p/-get-tag mv')]
      (if (= t mv'-tag)
        mv'
        (lifter/lift lifter t mv'-tag mv'))))
  (-return [m v]
    v))

(def ctx (->TaggedCtx nil nil))
