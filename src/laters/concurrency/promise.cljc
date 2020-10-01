(ns laters.concurrency.promise
  (:require
   [laters.concurrency.promise.protocols :as pr.p]))

(defn resolved
  [impl v]
  (pr.p/-resolved impl v))

(defn rejected
  [impl err]
  (pr.p/-rejected impl err))

(defn then
  [p f]
  (pr.p/-then p f))

(defn handle
  [p f]
  (pr.p/-handle p f))

(defn inspect
  [p]
  (pr.p/-handle p (fn [s e]
                    (if (some? e)
                      (prn "ERROR" e)
                      (prn "SUCCESS" s)))))

(defmacro pcatch
  "catch any exception and return as a rejected promise"
  [promise-impl & body]
  `(try
     ~@body
     (catch Exception x#
       (rejected ~promise-impl x#))))
