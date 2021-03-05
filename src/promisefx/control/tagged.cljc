(ns promisefx.control.tagged
  (:require
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.context.protocols :as ctx.p]
   [promisefx.context.lifter :as lifter]
   [promisefx.data.tagged :as tagged]
   [promisefx.data.tagged.protocols :as tagged.p]
   [promisefx.data.runnable.protocols :as runnable.p]))

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
    (let [mv-tag (tagged.p/-get-tag mv)]
      (if (= t mv-tag)
        (f mv)
        (f (lifter/lift lifter t mv-tag mv)))))
  (-return [m v]
    v))

(def ctx (->TaggedCtx [::Tagged] nil))


;; boxes the value in a BoxedTagged record.
;; use to test transformers, which must
;; also work with inner-ctx which introduce
;; additional structure
(deftype BoxedTaggedCtx [t lifter]
  ctx.p/Context
  (-get-tag [m] t)

  m.p/Monad
  (-bind [m mv f]
    (when-not (tagged/is-boxed-tagged? mv)
      (throw (ex-info "mv is not a BoxedTagged")))

    (let [{tag :tag val
           :val} mv]
      (if (= t tag)
        (f val)
        (f (lifter/lift lifter t tag val)))))
  (-return [m v]
    (tagged/->BoxedTagged t v)))

(def boxed-ctx (->BoxedTaggedCtx [::BoxedTagged] nil))
