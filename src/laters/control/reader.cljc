(ns laters.control.reader
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]))

(defprotocol MonadReader
  (-ask [m])
  (-local [m f mv]))

(deftype Reader [lifter]
  m.p/Monad
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

(defmacro ask
  ([m]
   `(-ask ~m))
  ([]
   `(-ask ~'this-monad##)))

(defmacro local
  ([m f mv]
   `(-local ~m ~f ~mv))
  ([f mv]
   `(-local ~'this-monad## ~f ~mv)))

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

(defmacro asks
  ([m f]
   `(-asks ~m ~f))
  ([f]
   `(-asks ~'this-monad## ~f)))

(def reader-ctx (Reader. nil))
(defn run-reader
  [wmv env]
  ((m/untag wmv) env))



(comment

  (m.reader/run-reader
   (m/mlet m.reader/reader-ctx
     [a (m.reader/ask)
      b (m/return 3)]
     (m/return (+ a b)))
   {:monad.reader/env 10})

  (m.reader/run-reader
   (m/mlet m.reader/reader-ctx
     [a (m.reader/asks :foo)
      b (m/return 3)]
     (m/return (* a b)))
   {:monad.reader/env {:foo 10}})

  (m.reader/run-reader
   (m/mlet m.reader/reader-ctx
     [a (m.reader/asks :foo)
      b (m.reader/local
         #(assoc % :foo 20)
         (m/mlet m.reader/reader-ctx
           [b (m.reader/asks :foo)]
           (m/return b)))]
     (m/return (* a b)))
   {:monad.reader/env {:foo 10}})
  )
