(ns laters.control.rwexception
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

(defrecord Failure [e]
  ctx.p/Extract
  (-extract [_] e))

(defn failure
  [e]
  (->Failure e))

(defn failure?
  [v]
  (instance? Failure v))

(defrecord RWExceptionVal [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  ctx.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn rwexception-val
  [ctx f]
  (->RWExceptionVal ctx f))

(defn rwexception-val?
  [v]
  (instance? RWExceptionVal v))

(defn plain-rwexception-val
  [ctx v]
  (rwexception-val
   ctx
   (fn [_]
     {:monad.writer/output nil
      :monad/val v})))

(defn error-rw-exception-body
  ([e] (error-rw-exception-body nil e))
  ([output e]
   (merge
    {:monad.writer/output nil}
    output
    {:monad/val (failure e)})))

(defn error-rwexception-val
  ([ctx e] (error-rwexception-val ctx nil e))
  ([ctx output e]
   (rwexception-val
    ctx
    (fn [_]
      (error-rw-exception-body output e)))))

(defn rw-exception-t-bind-2
  "pass both failure/left and right success/branches
   to the inner-2-mf... permits bind, catch, finally, handle
   behaviours to all use this same fn"
  ([output-ctx inner-ctx m inner-mv inner-2-mf]
   (rw-exception-t-bind-2 output-ctx inner-ctx m inner-mv false inner-2-mf))
  ([output-ctx inner-ctx m inner-mv discard-val? inner-2-mf]
   (m.p/-bind
    inner-ctx
    inner-mv

    (fn outer-mf [outer-mv]
      (assert (rwexception-val? outer-mv) (type outer-mv))

      (m.p/-return
       inner-ctx
       (rwexception-val
        m
        (fn [{env :monad.reader/env}]

          (try
            (let [{w :monad.writer/output
                   v :monad/val
                   :as r} (runnable.p/-run outer-mv {:monad.reader/env env})

                  [left right] (if (failure? v)
                                 [(ctx.p/-extract v) nil]
                                 [nil (ctx.p/-extract v)])]
              (try
                (let [inner-mv' (inner-2-mf left right)]

                  (m.p/-bind
                   inner-ctx
                   inner-mv'

                   (fn outer-mf' [outer-mv']
                     (assert (rwexception-val? outer-mv'))

                     (let [{w' :monad.writer/output
                            v' :monad/val} (runnable.p/-run
                                            outer-mv'
                                            {:monad.reader/env env})]
                       {:monad.writer/output (monoid/mappend
                                              output-ctx
                                              nil
                                              w
                                              w')
                        :monad/val (if discard-val? v v')}))))
                (catch Exception e
                  (error-rw-exception-body
                   {:monad.writer/output (monoid/mappend
                                          output-ctx
                                          nil
                                          w)}
                   e))))
            (catch Exception e
              (error-rw-exception-body e))))))))))

(deftype RWExceptionTCtx [output-ctx inner-ctx]
  ctx.p/Context
  (-get-type [m] (ctx.p/-get-type inner-ctx))
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (rw-exception-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     (fn [left right]
       (if (some? left)
         (err.p/-reject m left)
         (inner-mf right)))))

  (-return [m v]
    (m.p/-return inner-ctx (plain-rwexception-val m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (error-rwexception-val m v)))
  (-handle [m inner-mv inner-mf2]
    (rw-exception-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     inner-mf2))
  (-catch [m inner-mv inner-mf]
    ;; catch is like bind for failure
    (rw-exception-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     (fn [left right]
       (if (some? left)
         (inner-mf left)
         (m.p/-return m right)))))
  (-finally [m inner-mv inner-mf]
    ;; and finally is like bind for whatever
    (rw-exception-t-bind-2
     output-ctx
     inner-ctx
     m
     inner-mv
     true  ;; preserve the previous val
     (fn [_left _right]
       (inner-mf))))

  m.r.p/MonadReader
  (-ask [m]
    (m.p/-return
     inner-ctx
     (rwexception-val
      m
      (fn [{env :monad.reader/env}]
        {:monad.writer/output nil
         :monad/val env}))))
  (-local [m f mv]
    (m.p/-return
     inner-ctx
     (rwexception-val
      m
      (fn [{env :monad.reader/env}]
        (runnable.p/-run mv {:monad.reader/env (f env)})))))

  m.w.p/MonadWriter
  (-tell [m v]
    (m.p/-return
     inner-ctx
     (rwexception-val
      m
      (fn [{env :monad.reader/env}]
        {:monad.writer/output (monoid/mappend output-ctx nil v)}))))
  (-listen [m mv]
    (m.p/-return
     inner-ctx
     (rwexception-val
      m
      (fn [{env :monad.reader/env}]
        (let [{w :monad.writer/output
               :as lv} (runnable.p/-run mv {:monad.reader/env env})]
          {:monad.writer/output w
           :monad/val lv})))))
  (-pass [m mv]
    (m.p/-return
     inner-ctx
     (rwexception-val
      m
      (fn [{env :monad.reader/env}]
        (let [{w :monad.writer/output
               [val f] :monad/val} (runnable.p/-run mv {:monad.reader/env env})]
          {:monad.writer/output (f w)
           :monad/val val}))))))

(def rwexception-ctx
  (->RWExceptionTCtx
   monoid/map-monoid-ctx
   (id/->IdentityCtx [::RWExceptionT ::monoid/map ::id/Identity])))

(def tagged-rwexception-ctx
  (->RWExceptionTCtx
   monoid/map-monoid-ctx
   (tagged/->TaggedCtx [::RWExceptionT ::monoid/map ::tagged/Tagged] nil)))

(comment
  (require '[laters.control.rwexception :as rwx])
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.reader :as reader])
  (require '[laters.control.writer :as writer])
  (require '[laters.abstract.error :as e])
  (require '[laters.abstract.runnable :as r])

  (defn stuff
    [base]
    (m/mlet rwx/rwexception-ctx
      [_ (writer/tell [:base base])

       {offs :config/offset} (reader/ask)
       _ (writer/tell [:config/offset offs])

       tot (m/return (+ base offs))
       _ (writer/tell [:total tot])]

      ;; (throw (ex-info "boo" {}))

      (m/return tot)))

  (defn handle-stuff
    [& args]
    (m/mlet rwx/rwexception-ctx
      (e/handle
       (apply stuff args)
       (fn [left right]
         (if (some? left)
           (m/mlet rwx/rwexception-ctx
             [_ (writer/tell [:ex :handled])]
             (m/return (ex-message left)))

           (m/mlet rwx/rwexception-ctx
             [_ (writer/tell [:ex :none])]
             (m/return right)))))))

  ;; writer log is preserved through errors,
  ;; and you can log in handle, catch and finally

  (r/run (handle-stuff 100)
    {:monad.reader/env {:config/offset 100}})

  =>

  {:monad.writer/output {:base [100],
                         :config/offset [100],
                         :total [200],
                         :ex [:none]},
   :monad/val 200}

  ;; here's how an error gets caught/handled

  (r/run (handle-stuff 100)
    {:monad.reader/env {:config/offset :foo}})

  {:monad.writer/output {:base [100],
                         :config/offset [:foo],
                         :ex [:handled]},
   :monad/val "class clojure.lang.Keyword cannot be cast to class java.lang.Number (clojure.lang.Keyword is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')"}


  (r/run
    (m/mlet rwx/rwexception-ctx
      (e/finally
        (stuff 100)
        (fn [] (prn "finally") (writer/tell [:ex :finally]))))
    {:monad.reader/env {:config/offset 100}})

  =>

  {:monad.writer/output
   {:base [100],
    :config/offset [100],
    :total [200],
    :ex [:finally :caught]},
   :monad/val "boo"}

   )
