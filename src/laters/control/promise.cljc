(ns laters.control.promise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.abstract.error.protocols :as e.p]
   [laters.control.identity :as m.id]
   [laters.concurrency.promise :as p]
   [laters.concurrency.promise.promesa :as promesa]))


(deftype Promise [promise-impl lifter]
  m.p/Monad
  (-type [m]
    (into [::Promise] (p/type promise-impl)))
  (-bind [m tmv f]
    (let [mv (l/lift-untag lifter m tmv)]
      (t/tag
       m
       (p/then
        mv
        (fn [v]
          (l/lift-untag lifter m (f v)))
        promise-impl))))
  (-return [m v]
    (t/tag m (p/resolved promise-impl v)))
  e.p/MonadError
  (-reject [m v]
    (t/tag m (p/rejected promise-impl v)))
  (-catch [m mv handler]
    (prn "-catch" mv handler)
    (t/tag
     m
     (p/handle
      (l/lift-untag lifter m mv)
      (fn [success error]
        (if (some? error)
          (l/lift-untag lifter m (handler error))
          success))
      promise-impl))))

(defn promise-lifters
  [promise-impl]
  {[::m.id/Identity] (fn [mv]
                       (p/resolved
                        promise-impl
                        mv))})

(defn make-promise-ctx
  ([promise-impl]
   (let [lifter-registry (l/create-atomic-lifter-registry)
         ctx (make-promise-ctx promise-impl lifter-registry)]
     (l/register-all lifter-registry ctx (promise-lifters promise-impl))
     ctx))
  ([promise-impl lifter]
   (Promise. promise-impl lifter)))

(def promise-ctx (make-promise-ctx promesa/default-impl))




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
    ))
