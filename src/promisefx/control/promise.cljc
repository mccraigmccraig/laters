(ns promisefx.control.promise
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.fx.error.protocols :as err.p]
   [promisefx.data.exception :as data.ex]
   [promisefx.control.identity :as ctrl.id]
   [promisefx.control.tagged :as ctrl.tag]
   [promesa.core :as p]))

(deftype PromiseTCtx [inner-ctx]
  ctx.p/Context
  (-get-tag [m] (ctx.p/-get-tag inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (m.p/-bind
     inner-ctx
     inner-mv
     (fn outer-mf [outer-mv]
       (p/handle
        outer-mv
        (fn [right left]
          (if (some? left)
            (p/rejected left)
            (try
              (inner-mf right)
              (catch Exception x
                (p/rejected x)))))))))
  (-return [m v]
    (m.p/-return inner-ctx (p/resolved v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (p/rejected v)))
  (-catch [m inner-mv inner-mf]
    (m.p/-bind
     inner-ctx
     inner-mv
     (fn outer-mf [outer-mv]

       (p/handle
        outer-mv
        (fn [right left]
          (if (some? left)
            (try
              (inner-mf (data.ex/unwrap-exception left))
              (catch Exception e
                (p/rejected e)))
            right))))))
  (-finally [m inner-mv inner-mf]
    (m.p/-bind
     inner-ctx
     inner-mv
     (fn outer-mf [outer-mv]
       (p/finally
         outer-mv
         inner-mf)))))

(def ctx
  (->PromiseTCtx (ctrl.id/->IdentityCtx [::PromiseT ::ctrl.id/Identity])))

(def tagged-ctx
  (->PromiseTCtx (ctrl.tag/->TaggedCtx [::PromiseT ::ctrl.tag/Tagged] nil)))
