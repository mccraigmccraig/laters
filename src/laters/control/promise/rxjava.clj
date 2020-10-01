(ns laters.control.promise.rxjava
  (:require
   [laters.control.promise.protocols :as p])
  (:import
   [io.reactivex.subjects SingleSubject]
   [io.reactivex Single SingleObserver]
   [io.reactivex.functions Function]))

(deftype SingleSubjectPromiseFactory []
  p/IPromiseFactory
  (-resolved [ctx v]
    (let [ss (SingleSubject/create)]
      (.onSuccess ss v)
      ss))
  (-rejected [ctx err]
    (let [ss (SingleSubject/create)]
      (.onError err)
      ss)))

(defn ss-then
  [p f]
  (.map
   p
   (reify Function
     (apply [_ v] (f v)))))

(defn ss-handle
  [p f]
  (let [ss (SingleSubject/create)]
    (.subscribeWith
     p
     (reify SingleObserver
       (onSubscribe [_ _])
       (onError [_ err]
         (f nil err))
       (onSuccess [_ success]
         (f success nil))))
    ss))

(extend Single
  p/IPromise
  {:-then ss-then
   :-handle ss-handle})

(def singlesubject-Factory (SingleSubjectPromiseFactory.))
