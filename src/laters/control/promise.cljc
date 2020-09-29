(ns laters.control.promise
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.abstract.error.protocols :as e.p]
   [laters.control.identity :as m.id]
   [promesa.core :as p]))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [& body]
  `(try
     ~@body
     (catch Exception x#
       (p/rejected x#))))

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
  e.p/MonadError
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

(def promise-lifter
  {m.id/identity-ctx (fn [mv]
                       (p/resolved mv))})

(def promise-ctx
  (Promise. promise-lifter))

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
