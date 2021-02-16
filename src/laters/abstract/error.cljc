(ns laters.abstract.error
  (:require
   [laters.abstract.error.protocols :as p]))

(defmacro always-catch
  "catch an exception in the mv form, ensuring catch always catches"
  [m mv f]
  `(try
     (p/-catch ~m ~mv ~f)
     (catch Exception x#
       (p/-catch ~m (p/-reject ~m x#) ~f))))

(defmacro always-finally
  "catch (and rethrow) an exception in the mv form,
   ensuring the finally fn always gets called"
  [m mv f]
  `(try
     (p/-finally ~m ~mv ~f)
     (catch Throwable t#
       (p/-finally ~m (p/-reject ~m t#) ~f)
       (throw t#))))

(defmacro reject
  ([m v]
   `(p/-reject ~m ~v))
  ([v]
   `(p/-reject ~'this-monad## ~v)))

(defmacro catch
  ([m mv f]
   `(always-catch ~m ~mv ~f))
  ([mv f]
   `(always-catch ~'this-monad## ~mv ~f)))

(defmacro finally
  ([m mv f]
   `(always-finally ~m ~mv ~f))
  ([mv f]
   `(always-finally ~'this-monad## ~mv ~f)))

;; use ErrorMarker as a marker container for errors,
;; in the case that the type has no inbuilt error state
(defrecord ErrorMarker [e])

(defn error-marker [e]
  (->ErrorMarker e))

(defn error?
  [v]
  (instance? ErrorMarker v))
