(ns laters.control.reader
  (:require
   [laters.control.reader.protocols :as reader.p]))

(defmacro ask
  ([m]
   `(reader.p/-ask ~m))
  ([]
   `(reader.p/-ask ~'this-monad##)))

(defmacro local
  ([m f mv]
   `(reader.p/-local ~m ~f ~mv))
  ([f mv]
   `(reader.p/-local ~'this-monad## ~f ~mv)))

(defmacro asks
  ([m f]
   `(reader.p/-asks ~m ~f))
  ([f]
   `(reader.p/-asks ~'this-monad## ~f)))
