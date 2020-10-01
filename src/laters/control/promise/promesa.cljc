(ns laters.control.promise.promesa
  (:require
   [laters.control.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [java.util.concurrent CompletableFuture]))


(deftype PromesaPromiseFactory []
  p/IPromiseFactory
  (-resolved [ctx v]
    (promesa/resolved v))
  (-rejected [ctx err]
    (promesa/rejected err)))

(extend CompletableFuture
  p/IPromise
  {:-then promesa/then
   :-handle promesa/handle})

(def promesa-factory (PromesaPromiseFactory.))
