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
  ([m handler mv]
   `(p/-catch ~m ~handler (catch-reject ~m ~mv)))
  ([handler mv]
   `(p/-catch ~'this-monad## ~handler (catch-reject ~'this-monad## ~mv))))
