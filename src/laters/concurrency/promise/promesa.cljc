(ns laters.concurrency.promise.promesa
  (:require
   [laters.concurrency.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [java.util.concurrent CompletableFuture]))


(deftype PromesaPromiseFactory []
  p/IPromiseFactory
  (-type [ctx]
    [:PromesaPromise])
  (-resolved [ctx v]
    (promesa/resolved v))
  (-rejected [ctx err]
    (promesa/rejected err)))

(extend CompletableFuture
  p/IPromise
  {:-then promesa/then
   :-handle promesa/handle
   #?@(:clj [:-deref clojure.core/deref])
   })

(def factory (PromesaPromiseFactory.))
