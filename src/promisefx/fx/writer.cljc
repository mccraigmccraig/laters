(ns promisefx.fx.writer
  (:require
   [promisefx.fx.writer.protocols :as writer.p]))

(defmacro tell
  ([m v]
   `(writer.p/-tell ~m ~v))
  ([v]
   `(writer.p/-tell ~'this-context## ~v)))

(defmacro listen
  ([m mv]
   `(writer.p/-listen ~m ~mv))
  ([mv]
   `(writer.p/-listen ~'this-context## ~mv)))

(defmacro pass
  ([m mv]
   `(writer.p/-pass ~m ~mv))
  ([mv]
   `(writer.p/-pass ~'this-context## ~mv)))
