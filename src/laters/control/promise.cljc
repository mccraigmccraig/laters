(ns laters.control.promise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.abstract.error.protocols :as e.p]
   [laters.control.identity :as m.id]
   [laters.control.promise.protocols :as pr.p]
   [laters.control.promise.promesa :as promesa]))

(defn resolved
  [ctx v]
  (pr.p/-resolved ctx v))

(defn rejected
  [ctx err]
  (pr.p/-rejected ctx err))

(defn chain
  [ctx p & fs]
  (pr.p/-chain ctx p fs))

(defn handle
  [ctx p f]
  (pr.p/-handle ctx p f))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [promise-impl & body]
  `(try
     ~@body
     (catch Exception x#
       (rejected ~promise-impl x#))))

(deftype Promise [promise-impl lifter]
  m.p/Monad
  (-bind [m tmv f]
    (let [mv (l/lift-untag lifter m tmv)]
      (t/tag
       m
       (chain
        promise-impl
        mv
        (fn [v]
          (l/lift-untag lifter m (f v)))))))
  (-return [m v]
    (t/tag m (resolved promise-impl v)))
  e.p/MonadError
  (-reject [m v]
    (t/tag m (rejected promise-impl v)))
  (-catch [m handler mv]
    (t/tag
     m
     (handle
      promise-impl
      (l/lift-untag lifter m mv)
      (fn [success error]
        (if (some? error)
          (handler error)
          success))))))

(defn make-promise-lifter
  [promise-impl]
  {m.id/identity-ctx (fn [mv]
                       (resolved
                        promise-impl
                        mv))})

(defn make-promise-ctx
  [promise-impl lifter]
  (Promise. promise-impl lifter))

(def promise-lifter (make-promise-lifter promesa/promesa-promise))
(def promise-ctx (make-promise-ctx promesa/promesa-promise promise-lifter))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.error :as e])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.promise :as m.pr])


  (def mv (m/mlet m.pr/promise-ctx
            [a (m/return 2)
             b (m/return 3)]
            (m/return (* a b))))

  ;; auto-lifting
  (def mv (m/mlet m.pr/promise-ctx
            [a (m/return m.id/identity-ctx 100)
             b (m/return 3)]
            (m/return (* a b))))

  ;; catch
  (def emv (e/reject m.pr/promise-ctx (ex-info "boo" {:foo 10})))
  (def cemv
    (m/mlet m.pr/promise-ctx
      [a (e/catch
             (fn [e]
               [:error (-> e ex-data)])
             emv)]
      (m/return a)))

  ;; catch early errors
  (def cemv
    (e/catch
        m.pr/promise-ctx
        ex-data
      (m/mlet m.pr/promise-ctx
        (throw (ex-info "foo" {:foo 20}))))))
