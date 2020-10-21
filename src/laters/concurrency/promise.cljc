(ns laters.concurrency.promise
  (:refer-clojure :exclude [deref type])
  (:require
   [laters.concurrency.promise.protocols :as pr.p]))

(defn type
  [impl]
  (pr.p/-type impl))

(defn resolved
  [impl v]
  (pr.p/-resolved impl v))

(defn rejected
  [impl err]
  (pr.p/-rejected impl err))

(defn deferred
  [impl]
  (pr.p/-deferred impl))

(defn promise?
  [impl p]
  (pr.p/-promise? impl p))

(defn resolve!
  [p v]
  (pr.p/-resolve! p v))

(defn reject!
  [p err]
  (pr.p/-reject! p err))

(defn then
  ([p f]
   (pr.p/-then p f))
  ([p f impl]
   (pr.p/-then p f impl)))

(defn handle
  ([p f]
   (pr.p/-handle p f))
  ([p f impl]
   (pr.p/-handle p f impl)))

(defn timeout
  ([p t]
   (pr.p/-timeout p t))
  ([p t tv]
   (pr.p/-timeout p t tv))
  ([p t tv impl]
   (pr.p/-timeout p t tv impl)))

(defn inspect
  [p]
  (pr.p/-handle p (fn [s e]
                    (if (some? e)
                      (prn "ERROR" e)
                      (prn "SUCCESS" s)))))

#?(:clj
   (defn deref
     [p]
     (pr.p/-deref p)))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [promise-impl & body]
  `(try
     ~@body
     (catch Exception x#
       (rejected ~promise-impl x#))))
