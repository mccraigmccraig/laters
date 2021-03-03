(ns promisefx.control.promise
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.exception :as data.ex]
   [promisefx.data.runnable.protocols :as r.p]
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.fx.error.protocols :as err.p]
   [promesa.core :as p])
  #?(:clj
     (:import
      [java.util.concurrent CompletableFuture])))

(extend-protocol r.p/IRunnable
  CompletableFuture
  (-run [mv arg]
    mv))

(deftype PromiseCtx []
  ctx.p/Context
  (-get-tag [m] [::Promise])

  m.p/Monad
  (-bind [m mv mf]
    (p/handle
     mv
     (fn [right left]
       (if (some? left)
         (throw (data.ex/unwrap-exception left))
         (mf right)))))
  (-return [m v]
    (p/resolved v))

  err.p/MonadError
  (-reject [m v]
    (p/rejected v))
  (-catch [m mv mf]
    (p/handle
     mv
     (fn [right left]
       (if (some? left)
         (mf (data.ex/unwrap-exception left))
         ;; careful - returning a plain exception is interpreted
         ;; as an error
         (p/resolved right)))))
  (-finally [m mv mf]
    (p/finally mv mf)))

(def ctx
  (->PromiseCtx))
