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
   [laters.monoid :as monoid]
   [promesa.core :as p]))

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

(defn plain-rwpromise-val
  [ctx v]
  (rwpromise-val
   ctx
   (fn [_]
     (p/resolved
      {:monad.writer/output nil
       :monad/val v}))))

(defn rw-promise-t-bind-2
  ([output-ctx inner-ctx m inner-mv discard-val? inner-2-mf]
   (m.p/-bind
    inner-ctx
    inner-mv

    (fn outer-mf [outer-mv]
      (assert (rwpromise-val? outer-mv))

      (m.p/-return
       inner-ctx
       (rwpromise-val
        m
        (fn [{env :monad.reader/env}]

          (p/handle
           (runnable.p/-run outer-mv {:monad.reader/env env})
           (fn [{w :monad.writer/output
                v :monad/val
                :as right}
               left]
             (let [inner-mv' (inner-2-mf left v)]

               (m.p/-bind
                inner-ctx
                inner-mv'

                (fn outer-mf' [outer-mv']
                  (assert (rwpromise-val? outer-mv'))

                  (p/handle
                   (runnable.p/-run outer-mv' {:monad.reader/env env})
                   (fn [{w' :monad.writer/output
                        v' :monad/val
                        :as right}
                       left]

                     (if (some? left)

                       ;;TODO no so simple ... left val is probably an exception ... and on jvm
                       ;; needs to be an exception, so need to thread all the
                       ;; effects onto an atom in an ex-info
                       {:monad.writer/output (monoid/mappend
                                              output-ctx
                                              nil w)
                        :monad/val nil}


                       {:monad.writer/output (monoid/mappend
                                              output-ctx
                                              nil
                                              w
                                              w')
                        :monad/val (if discard-val? v v')})



                     ))
                  ))


               ))))))))))

(deftype RWPromiseTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (rw-promise-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       (prn left right)
       (if (some? left)
         (err.p/-reject m left)
         (inner-mf right))))
    )

  (-return [m v]
    (m.p/-return inner-ctx (plain-rwpromise-val m v)))

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


(comment
  (require '[laters.control.rwpromise :as rwp])
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.reader :as reader])
  (require '[laters.control.writer :as writer])
  (require '[laters.abstract.error :as e])
  (require '[laters.abstract.runnable :as r])


  (def mv2 (m/bind
            rwp/rwpromise-ctx
            (m/return rwp/rwpromise-ctx 10)
            (fn [a] (m/return
                    rwp/rwpromise-ctx
                    (inc a)))))

  @(r/run mv2 {:monad.reader/env 10})

  )
