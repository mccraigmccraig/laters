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
