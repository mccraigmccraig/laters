(ns laters.control.promise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   ;; [laters.abstract.lifter :as l]
   [laters.abstract.tagged :as t]
   [laters.abstract.error.protocols :as e.p]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.control.identity :as m.id]
   [promesa.core :as promesa])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Promise context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Promise [executor]
  m.p/Monad
  (-type [m]
    [::Promise])
  (-bind [m mv f]
    (if (some? executor)
      (promesa/then mv f executor)
      (promesa/then mv f)))
  (-return [m v]
    (promesa/resolved v))

  e.p/MonadError
  (-reject [m v]
    (promesa/rejected v))
  (-catch [m mv err-handler]
    (prn "-catch" mv err-handler)
    (let [handler (fn [success error]
                    (if (some? error)
                      (err-handler error)
                      success))]
      (if (some? executor)
        (promesa/handle mv handler executor)
        (promesa/handle mv handler)))))

(defn make-promise-ctx
  [executor]
  (Promise. executor))

(def promise-ctx (make-promise-ctx nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TaggedPromise context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^ITaggedMv tagged-return
  [^ITaggedCtx ctx v]
  (let [umv (m.p/-return (tag.p/-wrapped-ctx ctx) v)]
    (t/tag ctx umv)))

(defn ^ITaggedMv tagged-promise-join
  "takes Promise<Tag<Promise<val>>> (from a wrapped-ctx bind of
   a tagged-ctx monadic-function) and flattens to
   Tag<Promise<val>>"
  [^ITaggedCtx ctx tv-p]
  (t/tag
   ctx
   (promesa/then
    tv-p
    (fn [tv]
      ;; tv is the result of the mf - do any lifting to the
      ;; required ctx here
      (t/untag tv)))))

(defn ^ITaggedMv tagged-promise-bind
  [^ITaggedCtx ctx ^ITaggedMv tmv tmf]
  (let [mv (t/untag tmv)
        ;; this wraps the outer tagged-mv in another promise
        ;; which promesa can't join because of the tag
        r (m.p/-bind (tag.p/-wrapped-ctx ctx) mv tmf)]

    ;; so we explicitly join
    (tagged-promise-join ctx r)))

(defn ^ITaggedMv tagged-promise-reject
  [^ITaggedCtx ctx v]
  (t/tag ctx (e.p/-reject (tag.p/-wrapped-ctx ctx) v)))

(defn ^ITaggedMv tagged-promise-catch
  [^ITaggedCtx ctx tmv h]
  (let [mv (t/untag tmv)
        r (e.p/-catch (tag.p/-wrapped-ctx ctx) mv h)]
    (tagged-promise-join ctx r)))

(deftype TaggedPromise [promise-opts lifter]
  tag.p/ITaggedCtx
  (-wrapped-ctx [this] promise-ctx)

  m.p/Monad
  (-bind [this tmv tmf]
    (tagged-promise-bind this tmv tmf))
  (-return [this v]
    (tagged-return this v))

  e.p/MonadError
  (-reject [this v]
    (tagged-promise-reject this v))
  (-catch [this tmv handler]
    (tagged-promise-catch this tmv handler)))


(defn make-tagged-promise-ctx
  [promise-opts lifter]
  (TaggedPromise. promise-opts lifter))

(def tagged-promise-ctx
  (make-tagged-promise-ctx {} nil))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.error :as e])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.promise :as m.pr])

  ;; plain context

  (def mv (m/mlet m.pr/promise-ctx
            [a (m/return 2)
             b (m/return 3)]
            (m/return (* a b))))

  ;; catch
  (def emv (e/reject m.pr/promise-ctx (ex-info "boo" {:foo 10})))
  (def cemv
    (m/mlet m.pr/promise-ctx
      [a (e/catch
             emv
             (fn [e]
               (m/return
                [:error (-> e ex-data)])))]
      (m/return a)))

  ;; catch early errors
  (def cemv
    (m/mlet m.pr/promise-ctx
      (e/catch
          (throw (ex-info "foo" {:foo 20}))
          #(m/return (ex-data %))))
    )

  ;; tagged context


  (def mv (m/mlet m.pr/tagged-promise-ctx
            [a (m/return 2)
             b (m/return 3)]
            (m/return (* a b))))

  ;; catch
  (def emv (e/reject m.pr/tagged-promise-ctx (ex-info "boo" {:foo 10})))
  (def cemv
    (m/mlet m.pr/tagged-promise-ctx
      [a (e/catch
             emv
             (fn [e]
               (m/return
                [:error (-> e ex-data)])))]
      (m/return a)))

  ;; catch early errors
  (def cemv
    (m/mlet m.pr/tagged-promise-ctx
      (e/catch
          (throw (ex-info "foo" {:foo 20}))
          #(m/return (ex-data %))))
    )



  )
