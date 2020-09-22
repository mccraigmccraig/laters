(ns laters.control.promise
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [promesa.core :as p]))

(deftype Promise [lifters]
  m/Monad
  (-bind [m wmv f]
    (let [wmv (m/-lift m wmv)]
      (m/tag
       m
       (p/chain
        (m/untag wmv)
        f
        :mv))))
  (-return [m v]
    (m/tag m (p/resolved v)))
  (-lift [m wmv]
    (m/lift m lifters wmv)))

(def promise-lifters
  {m.id/identity-ctx (fn [mv]
                       (p/resolved mv))})

(def promise-ctx
  (Promise. promise-lifters))

(comment

  )
