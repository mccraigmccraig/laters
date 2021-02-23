(ns laters.abstract.error
  (:require
   [laters.abstract.error.protocols :as p]
   [laters.abstract.context.protocols :as ctx.p]))

(defmacro always-catch
  "catch an exception in the mv form, ensuring catch always catches"
  [m mv f]
  `(try
     (p/-catch ~m ~mv ~f)
     (catch Exception x#
       (p/-catch ~m (p/-reject ~m x#) ~f))))

(defmacro always-handle
  "catch an exception in the mv form, ensuring handle always handles"
  [m mv f2]
  `(try
     (p/-handle ~m ~mv ~f2)
     (catch Exception x#
       (p/-handle ~m (p/-reject ~m x#) ~f2))))

(defmacro always-finally
  "catch (and rethrow) an exception in the mv form,
   ensuring the finally fn always gets called"
  [m mv f]
  `(try
     (p/-finally ~m ~mv ~f)
     (catch Throwable t#
       (p/-finally ~m (p/-reject ~m t#) ~f)
       (throw t#))))

(defn reject'
  [m v]
  (p/-reject m v))

(defmacro reject
  ([m v]
   `(p/-reject ~m ~v))
  ([v]
   `(p/-reject ~'this-context## ~v)))

(defn handle'
  ([m mv f2]
   (p/-handle m mv f2))
  ([mv f2]
   (p/-handle (ctx.p/-get-context mv) mv f2)))

(defmacro handle
  ([m mv f2]
   `(always-handle ~m ~mv ~f2))
  ([mv f2]
   `(always-handle ~'this-context## ~mv ~f2)))

(defn catch'
  ([m mv f2]
   (p/-catch m mv f2))
  ([mv f2]
   (p/-catch (ctx.p/-get-context mv) mv f2)))

(defmacro catch
  ([m mv f]
   `(always-catch ~m ~mv ~f))
  ([mv f]
   `(always-catch ~'this-context## ~mv ~f)))

(defn finally'
  ([m mv f2]
   (p/-finally m mv f2))
  ([mv f2]
   (p/-finally (ctx.p/-get-context mv) mv f2)))

(defmacro finally
  ([m mv f]
   `(always-finally ~m ~mv ~f))
  ([mv f]
   `(always-finally ~'this-context## ~mv ~f)))
