(ns laters.control.reader
  (:require
   [laters.abstract.monad :as m]))

(defprotocol MonadReader
  (-ask [m])
  (-local [m f mv]))

(deftype Reader [lifter]
  m/Monad
  (-bind [m mv f]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (let [{v :monad/val} ((m/lift-untag lifter m mv)
                             {:monad.reader/env env})]
         ((m/lift-untag lifter m (f v))
          {:monad.reader/env env})))))
  (-return [m v]
    (m/tag m (fn [_] {:monad/val v})))
  MonadReader
  (-ask [m]
    (m/tag m (fn [{env :monad.reader/env}] {:monad/val env})))
  (-local [m f mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       ((m/lift-untag lifter m mv) {:monad.reader/env (f env)})))))

(defn -asks
  [m f]
  (m/tag
   m
   (fn [{env :monad.reader/env :as arg}]
     ;; note this assocs the modified :monad.reader/env to
     ;; the arg map, rather than constructing a new arg map,
     ;; so will work with e.g. RWS monad too
     ;; which will pass :monad.state/state in arg
     ((m/untag (-ask m)) (assoc arg :monad.reader/env (f env))))))

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
   {:monad.reader/env 10})

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (asks :foo)
      b (return 3)]
     (return (* a b)))
   {:monad.reader/env {:foo 10}})

  (m/run-reader
   (m/mlet m.reader/reader-ctx
     [a (asks :foo)
      b (local
         #(assoc % :foo 20)
         (m/mlet m.reader/reader-ctx
           [b (asks :foo)]
           (return b)))]
     (return (* a b)))
   {:monad.reader/env {:foo 10}})
  )
