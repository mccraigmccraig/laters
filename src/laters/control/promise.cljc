(ns laters.control.promise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.identity :as m.id]
   [promesa.core :as p]))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [& body]
  `(try
     ~@body
     (catch Exception x#
       (p/rejected x#))))

(defprotocol MonadPromise
  (-reject [m v])
  (-catch [m handler mv]))

(defmacro catch-reject
  [m & body]
  `(try
     ~@body
     (catch Exception x#
       (-reject ~m x#))))

(deftype Promise [lifter]
  m.p/Monad
  (-bind [m tmv f]
    (let [mv (l/lift-untag lifter m tmv)]
      (t/tag
       m
       (p/chain
        mv
        (fn [v]
          (l/lift-untag lifter m (f v)))))))
  (-return [m v]
    (t/tag m (p/resolved v)))
  MonadPromise
  (-reject [m v]
    (t/tag m (p/rejected v)))
  (-catch [m handler mv]
    (t/tag
     m
     (p/handle
      (l/lift-untag lifter m mv)
      (fn [success error]
        (if (some? error)
          (handler error)
          success))))))

(defmacro reject
  ([m v]
   `(-reject ~m ~v))
  ([v]
   `(-reject ~'this-monad## ~v)))

(defmacro catch
  ([m handler mv]
   `(-catch ~m ~handler (catch-reject ~m ~mv)))
  ([handler mv]
   `(-catch ~'this-monad## ~handler (catch-reject ~'this-monad## ~mv))))

(def promise-lifter
  {m.id/identity-ctx (fn [mv]
                       (p/resolved mv))})

(def promise-ctx
  (Promise. promise-lifter))

(comment
  (require '[laters.abstract.monad :as m])
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
  (def emv (m.pr/reject m.pr/promise-ctx (ex-info "boo" {:foo 10})))
  (def cemv
    (m/mlet m.pr/promise-ctx
      [a (m.pr/catch
             (fn [e]
               [:error (-> e ex-data)])
             emv)]
      (m/return a)))

  )
