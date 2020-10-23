(ns laters.concurrency.promise.promesa
  (:require
   [laters.concurrency.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [java.util.concurrent CompletableFuture]))

(deftype PromesaPromiseImpl [executor]
  p/IPromiseImpl
  (-type [ctx]
    [:PromesaPromise])
  (-resolved [_ v]
    (promesa/resolved v))
  (-rejected [_ err]
    (promesa/rejected err))
  (-deferred [_]
    (promesa/deferred))
  (-promise? [_ p]
    (promesa/promise? p)))

(defn promesa-then
  ([p f]
   (promesa/then p f))
  ([p f ^PromesaPromiseImpl impl]
   (if-let [x (some-> impl .executor)]
     (promesa/then p f x)
     (promesa/then p f))))

(defn promesa-handle
  ([p f]
   (promesa/handle p f))
  ([p f ^PromesaPromiseImpl impl]
   (if-let [x (some-> impl .executor)]
     (promesa/handle p f x)
     (promesa/handle p f))))

(defn promesa-timeout
  ([p t]
   (promesa/timeout p t))
  ([p t v]
   (promesa/timeout p t v))
  ([p t v ^PromesaPromiseImpl impl]
   (if-let [x (some-> impl .executor)]
     (promesa/handle p t v x)
     (promesa/timeout p t v))))

(extend CompletableFuture
  p/IPromise
  {:-then promesa-then
   :-handle promesa-handle
   :-timeout promesa-timeout
   :-resolve! promesa/resolve!
   :-reject! promesa/reject!
   #?@(:clj [:-deref clojure.core/deref])
   })

(defn make-promesa-promise-impl
  [executor]
  (PromesaPromiseImpl. executor))

(def default-impl (make-promesa-promise-impl nil))
