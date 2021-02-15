(ns laters.abstract.error
  (:require
   [laters.abstract.error.protocols :as p]
   [laters.abstract.tagged :as tag]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as lift]
   [laters.abstract.monad.protocols :as m.p]))

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


(defn tagged-reject
  [m v]
  (tag/tag m
           (p/-reject
            (tag.p/-inner-ctx m)
            v)))

(defn tagged-catch
  [m mv f lifter]
  (let [i-ctx (tag.p/-inner-ctx m)
        tcmv (p/-catch i-ctx mv f)]
    (tag/tag
     m
     (lift/lift lifter (m.p/-type m) (m.p/-type i-ctx) (tag/untag tcmv)))))

(defn tagged-finally
  [m mv f]
  (let [i-ctx (tag.p/-inner-ctx m)]
    (p/-finally i-ctx mv f)))
