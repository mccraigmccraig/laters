(ns laters.control.promise
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [promesa.core :as p]))

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
    (m/tag m (p/resolved v))))

(def promise-lifter
  {m.id/identity-ctx (fn [mv]
                       (p/resolved mv))})

(def promise-ctx
  (Promise. promise-lifter))

(comment

  (def mv (m/mlet m.pr/promise-ctx
            [a (return 2)
             b (return 3)]
            (return (* a b))))

  ;; auto-lifting
  (def mv (m/mlet m.pr/promise-ctx
            [a (m/-return m.id/identity-ctx 100)
             b (return 3)]
            (return (* a b))))
  )
