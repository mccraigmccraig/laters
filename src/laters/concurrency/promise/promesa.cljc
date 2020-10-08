(ns laters.concurrency.promise.promesa
  (:require
   [laters.concurrency.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [java.util.concurrent CompletableFuture]))


(deftype PromesaPromiseFactory [executor]
  p/IPromiseFactory
  (-type [ctx]
    [:PromesaPromise])
  (-executor [_]
    executor)
  (-resolved [_ v]
    (promesa/resolved v))
  (-rejected [_ err]
    (promesa/rejected err))
  (-deferred [_]
    (promesa/deferred)))

(extend CompletableFuture
  p/IPromise
  {:-then promesa/then
   :-handle promesa/handle
   #?@(:clj [:-deref clojure.core/deref])
   })

(def factory (PromesaPromiseFactory. nil))
