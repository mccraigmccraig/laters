(ns laters.control.reader
  (:require
   [laters.abstract.monad :as m]))

(defprotocol MonadReader
  (-ask [m]))

(deftype Reader []
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [env]
       (let [v ((m/untag wmv) env)]
         ((m/untag (f v)) env)))))
  (-return [m v]
    (m/tag
     m
     (fn [env]
       v)))
  MonadReader
  (-ask [m]
    (fn [] (m/tag m (fn [env] env)))))

(defmethod m/-lets (.getName Reader)
  [_ m]
  `[~'ask (-ask ~m)])

(def reader-ctx (Reader.))
(defn run-reader
  [wmv env]
  ((m/untag wmv) env))



(comment

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (ask)
      b (return 3)]
     (return (+ a b)))
   10))
