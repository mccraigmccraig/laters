(ns laters.control.reader
  (:require
   [laters.abstract.monad :as m]))

(defprotocol MonadReader
  (-ask [m])
  (-asks [m f])
  (-local [m f mv]))

(deftype Reader [lifter]
  m/Monad
  (-bind [m tmv f]
    (m/tag
     m
     (fn [env]
       (let [v ((m/lift-untag lifter m tmv) env)]
         ((m/lift-untag lifter m (f v)) env)))))
  (-return [m v]
    (m/tag
     m
     (fn [env]
       v)))
  MonadReader
  (-ask [m]
    (m/tag m (fn [env] env)))
  (-asks [m f]
    (m/tag m (fn [env] (f env))))
  (-local [m f mv]
    (m/tag
     m
     (fn [env]
       ((m/lift-untag lifter m mv) (f env))))))

(defmethod m/-lets (.getName Reader)
  [_ m]
  `[~'ask (fn [] (-ask ~m))
    ~'asks (fn [f#] (-asks ~m f#))
    ~'local (fn [f# mv#] (-local ~m f# mv#))])

(def reader-ctx (Reader. nil))
(defn run-reader
  [wmv env]
  ((m/untag wmv) env))



(comment

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (ask)
      b (return 3)]
     (return (+ a b)))
   10)

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (asks :foo)
      b (return 3)]
     (return (* a b)))
   {:foo 10})

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (asks :foo)
      b (local
         #(assoc % :foo 20)
         (m/mlet m.reader/reader-ctx
           [b (asks :foo)]
           (return b)))]
     (return (* a b)))
   {:foo 10})
  )
