(ns laters.abstract.tagged
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.lifter :as lifter]
   [laters.abstract.context.protocols :as ctx.p]))

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
  (-get-type [m] t)

  m.p/Monad
  (-bind [m mv f]
    (let [mv' (f mv)
          mv'-tag (ctx.p/-get-type (ctx.p/-get-context mv'))]
      (if (= t mv'-tag)
        mv'
        (lifter/lift lifter t mv'-tag mv'))))
  (-return [m v]
    v))

(def tagged-ctx (->TaggedCtx nil nil))
