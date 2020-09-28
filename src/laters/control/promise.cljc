(ns laters.control.promise
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [promesa.core :as p]))

(defprotocol MonadPromise
  (-reject [m v])
  (-catch [m handler mv]))

(deftype Promise [lifter]
  m/Monad
  (-bind [m tmv f]
    (let [mv (m/lift-untag lifter m tmv)]
      (m/tag
       m
       (p/chain
        mv
        (fn [v]
          (m/lift-untag lifter m (f v)))))))
  (-return [m v]
    (m/tag m (p/resolved v)))
  (-reject [m v]
    (m/tag m (p/rejected v)))
  MonadPromise
  (-catch [m handler mv]
    (m/tag
     m
     (p/handle
      (m/lift-untag lifter m mv)
      (fn [success error]
        (if (some? error)
          (handler error)
          success))))))

(defmacro reject
  [v]
  `(-reject ~'this-monad## ~v))

(defmacro catch
  [handler mv]
  `(-catch ~'this-monad## ~handler ~mv))

(def promise-lifter
  {m.id/identity-ctx (fn [mv]
                       (p/resolved mv))})

(def promise-ctx
  (Promise. promise-lifter))

(comment

  (def mv (m/mlet m.pr/promise-ctx
            [a (m/return 2)
             b (m/return 3)]
            (m/return (* a b))))

  ;; auto-lifting
  (def mv (m/mlet m.pr/promise-ctx
            [a (m/-return m.id/identity-ctx 100)
             b (m/return 3)]
            (m/return (* a b))))

  ;; catch
  (def emv (m.pr/-reject m.pr/promise-ctx (ex-info "boo" {:foo 10})))
  (def cemv
    (m/mlet m.pr/promise-ctx
      [a (m.pr/catch
             (fn [e]
               [:error (-> e ex-data)])
             emv)]
      (m/return a)))

  )
