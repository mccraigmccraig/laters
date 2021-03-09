(ns promisefx.control.rwsx
  (:require
   [promisefx.context.protocols :as ctx.p]
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.fx.monad :as m]
   [promisefx.fx.error.protocols :as err.p]
   [promisefx.fx.reader.protocols :as m.r.p]
   [promisefx.fx.writer.protocols :as m.w.p]
   [promisefx.control.identity :as ctrl.id]
   [promisefx.control.tagged :as ctrl.tag]
   [promisefx.data.extractable.protocols :as extractable.p]
   [promisefx.data.runnable.protocols :as runnable.p]
   [promisefx.data.monoid :as monoid]
   [promisefx.data.success-failure :as s.f]
   [promisefx.data.tagged.protocols :as tagged.p]))

;; values are: <env> -> <writer,success|failure>
(defrecord RWSXVal [ctx f]
  ctx.p/Contextual
  (-get-context [_] ctx)
  extractable.p/Extract
  (-extract [_] f)
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg))
  tagged.p/Tagged
  (-get-tag [_]
    (ctx.p/-get-tag ctx)))

(defn rwsx-val
  [ctx f]
  (->RWSXVal ctx f))

(defn rwsx-val?
  [v]
  (instance? RWSXVal v))

(defn success-rwsx-val
  [ctx v]
  (rwsx-val
   ctx
   (fn [_]
     {:promisefx.writer/output nil
      :promisefx/val v})))

(defn failure-rwsx-body
  ([ctx e] (failure-rwsx-body ctx nil e))
  ([ctx output e]
   (merge
    {:promisefx.writer/output nil}
    output
    {:promisefx/val (s.f/failure ctx e)})))

(defn failure-rwsx-val
  ([ctx e] (failure-rwsx-val ctx nil e))
  ([ctx output e]
   (rwsx-val
    ctx
    (fn [_]
      (failure-rwsx-body ctx output e)))))

(defn rw-exception-t-bind-2
  "pass both failure/left and right success/branches
   to the inner-2-mf... permits bind, catch, finally, handle
   behaviours to all use this same fn"
  [outer-ctx output-ctx inner-ctx m inner-mv discard-val? inner-2-mf]
 (m.p/-bind
    inner-ctx
    inner-mv

    (fn outer-mf [outer-mv]
      (assert (rwsx-val? outer-mv) (type outer-mv))

      (m.p/-return
       inner-ctx
       (rwsx-val
        m
        (fn [{env :promisefx.reader/env}]

          (try
            (let [{w :promisefx.writer/output
                   v :promisefx/val} (try
                                   (runnable.p/-run
                                    outer-mv
                                    {:promisefx.reader/env env})
                                   (catch #?(:clj Exception :cljs :default) e
                                     ;; catch and thread errors forward
                                     (failure-rwsx-body outer-ctx e)))

                  [left right] (if (s.f/failure? v)
                                 [(extractable.p/-extract v) nil]
                                 [nil (extractable.p/-extract v)])

                  inner-mv' (try
                              (inner-2-mf left right)
                              (catch #?(:clj Exception :cljs :default) e
                                ;; catch and thread errors forward
                                (failure-rwsx-val
                                 outer-ctx
                                 {:promisefx.writer/output (monoid/mappend
                                                        output-ctx
                                                        nil
                                                        w)}
                                 e)))]

              (m.p/-bind
                   inner-ctx
                   inner-mv'

                   (fn outer-mf' [outer-mv']
                     (assert (rwsx-val? outer-mv'))

                     (let [{w' :promisefx.writer/output
                            v' :promisefx/val} (try
                                             (runnable.p/-run
                                              outer-mv'
                                              {:promisefx.reader/env env})
                                             (catch #?(:clj Exception :cljs :default) e
                                               ;; catch and thread errors forward
                                               (failure-rwsx-body
                                                outer-ctx
                                                {:promisefx.writer/output (monoid/mappend
                                                                       output-ctx
                                                                       nil
                                                                       w)}
                                                e)))]
                       {:promisefx.writer/output (monoid/mappend
                                              output-ctx
                                              nil
                                              w
                                              w')
                        :promisefx/val (if discard-val? v v')}))))
            (catch #?(:clj Exception :cljs :default) e
              ;; final fallback
              (failure-rwsx-body outer-ctx e)))))))))

(deftype RWExceptionTCtx [tag output-ctx inner-ctx]
  ctx.p/Context
  (-get-tag [m] tag)
  m.p/Monad
  (-bind [m inner-mv inner-mf]
    (rw-exception-t-bind-2
     m
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       (if (some? left)
         (err.p/-reject m left)
         (inner-mf right)))))

  (-return [m v]
    (m.p/-return inner-ctx (success-rwsx-val m v)))

  err.p/MonadError
  (-reject [m v]
    (m.p/-return inner-ctx (failure-rwsx-val m v)))
  (-handle [m inner-mv inner-mf2]
    (rw-exception-t-bind-2
     m
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     inner-mf2))
  (-catch [m inner-mv inner-mf]
    ;; catch is like bind for failure
    (rw-exception-t-bind-2
     m
     output-ctx
     inner-ctx
     m
     inner-mv
     false
     (fn [left right]
       (if (some? left)
         (inner-mf left)
         (m.p/-return m right)))))
  (-finally [m inner-mv inner-mf]
    ;; and finally is like bind for whatever
    (rw-exception-t-bind-2
     m
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
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        {:promisefx.writer/output nil
         :promisefx/val env}))))
  (-asks [m f]
    (m.p/-return
     inner-ctx
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        {:promisefx.writer/output nil
         :promisefx/val (f env)}))))
  (-local [m f mv]
    (m.p/-return
     inner-ctx
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        (runnable.p/-run mv {:promisefx.reader/env (f env)})))))

  m.w.p/MonadWriter
  (-tell [m v]
    (m.p/-return
     inner-ctx
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        {:promisefx.writer/output (monoid/mappend output-ctx nil v)
         :promisefx/val nil}))))
  (-listen [m mv]
    (m.p/-return
     inner-ctx
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        (let [{w :promisefx.writer/output
               :as lv} (runnable.p/-run mv {:promisefx.reader/env env})]
          {:promisefx.writer/output w
           :promisefx/val lv})))))
  (-pass [m mv]
    (m.p/-return
     inner-ctx
     (rwsx-val
      m
      (fn [{env :promisefx.reader/env}]
        (let [{w :promisefx.writer/output
               [val f] :promisefx/val} (runnable.p/-run mv {:promisefx.reader/env env})]
          {:promisefx.writer/output (f w)
           :promisefx/val val}))))))

(def ctx
  (->RWExceptionTCtx
   [::RWExceptionT ::monoid/map ::ctrl.id/IdentityCtx]
   monoid/map-monoid-ctx
   (ctrl.id/->IdentityCtx)))

(def tagged-ctx
  (->RWExceptionTCtx
   [::RWExceptionT ::monoid/map ::ctrl.tag/TaggedCtx]
   monoid/map-monoid-ctx
   (ctrl.tag/->TaggedCtx [::RWExceptionT ::monoid/map ::ctrl.tag/TaggedCtx] nil)))

(def boxed-tagged-ctx
  (->RWExceptionTCtx
   [::RWExceptionT ::monoid/map ::ctrl.tag/BoxedTagged]
   monoid/map-monoid-ctx
   (ctrl.tag/->BoxedTaggedCtx [::RWExceptionT ::monoid/map ::ctrl.tag/BoxedTagged] nil)))

(comment
  (require '[promisefx.context :as ctx])
  (require '[promisefx.control.rwexception :as rwx])
  (require '[promisefx.abstract.monad :as m])
  (require '[promisefx.control.reader :as reader])
  (require '[promisefx.control.writer :as writer])
  (require '[promisefx.abstract.error :as e])
  (require '[promisefx.abstract.runnable :as r])

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
    (ctx/with-context rwx/rwexception-ctx
      (-> (apply stuff args)
          (e/handle
           (fn [left right]
             (if (some? left)
               (m/mlet
                 [_ (writer/tell [:ex :handled])]
                 (m/return (ex-message left)))

               (m/mlet
                 [_ (writer/tell [:ex :none])]
                 (m/return right))))))))

  ;; writer log is preserved through errors,
  ;; and you can log in handle, catch and finally

  (r/run (handle-stuff 100)
    {:promisefx.reader/env {:config/offset 100}})

  =>

  {:promisefx.writer/output {:base [100],
                         :config/offset [100],
                         :total [200],
                         :ex [:none]},
   :promisefx/val 200}

  ;; here's how an error gets caught/handled

  (r/run (handle-stuff 100)
    {:promisefx.reader/env {:config/offset :foo}})

  {:promisefx.writer/output {:base [100],
                         :config/offset [:foo],
                         :ex [:handled]},
   :promisefx/val "class clojure.lang.Keyword cannot be cast to class java.lang.Number (clojure.lang.Keyword is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')"}


  (r/run
    (m/mlet rwx/rwexception-ctx
      (e/finally
        (stuff 100)
        (fn [] (prn "finally") (writer/tell [:ex :finally]))))
    {:promisefx.reader/env {:config/offset 100}})

  =>

  {:promisefx.writer/output
   {:base [100],
    :config/offset [100],
    :total [200],
    :ex [:finally :caught]},
   :promisefx/val "boo"}

   )
