(ns laters.abstract.error
  (:require
   [laters.abstract.error.protocols :as p]))

(defmacro catch-reject
  [m & body]
  `(try
     ~@body
     (catch Exception x#
       (p/-reject ~m x#))))

(defmacro reject
  ([m v]
   `(p/-reject ~m ~v))
  ([v]
   `(p/-reject ~'this-monad## ~v)))

(defmacro catch
  ([m mv handler]
   `(p/-catch ~m (catch-reject ~m ~mv) ~handler ))
  ([mv handler]
   `(p/-catch ~'this-monad## (catch-reject ~'this-monad## ~mv) ~handler)))
