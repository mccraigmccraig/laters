(ns laters.control.rwpromise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.context.protocols :as ctx.p]
   [laters.abstract.monad :as m]
   [laters.abstract.error.protocols :as err.p]
   [laters.abstract.error :as error]
   [laters.control.reader.protocols :as m.r.p]
   [laters.control.writer.protocols :as m.w.p]
   [laters.control.maybe :as maybe]
   [laters.control.identity :as id]
   [laters.abstract.tagged :as tagged]
   [laters.abstract.runnable.protocols :as runnable.p]
   [laters.monoid :as monoid]))

;; values are: <env> -> Promise<writer,val>
(defrecord RWPromiseVal [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn rwpromise-val
  [ctx f]
  (->RWPromiseVal ctx f))

(defn rwpromise-val?
  [v]
  (instance? RWPromiseVal v))

(deftype RWPromiseTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    )

  (-return [m v]
    )

  err.p/MonadError
  (-reject [m v]
    )
  (-handle [m inner-mv inner-mf2]
    )
  (-catch [m inner-mv inner-mf]
    )
  (-finally [m inner-mv inner-mf]
    )

  m.r.p/MonadReader
  (-ask [m]
    )
  (-local [m f mv]
    )

  m.w.p/MonadWriter
  (-tell [m v]
    )
  (-listen [m mv]
    )
  (-pass [m mv]
    ))

(def rwpromise-ctx
  (->RWPromiseTCtx
   monoid/map-monoid-ctx
   (id/->IdentityCtx [::RWPromiseT ::monoid.map ::id/identity])))

(def tagged-rwpromise-ctx
  (->RWPromiseTCtx
   monoid/map-monoid-ctx
   (tagged/->TaggedCtx [::RWPromiseT ::monoid.map ::tagged/Tagged] nil)))
