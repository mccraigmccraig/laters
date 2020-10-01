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
  (.flatMap
   p
   (reify Function
     (apply [_ v] (f v)))))

(defn ss-flatten
  [out p]
  (if (not (instance? Single p))
    (.onSuccess out p)

    (.subscribeWith
     p
     (reify SingleObserver
       (onSubscribe [_ _])
       (onError [_ err]
         (.onError out err))
       (onSuccess [_ success]
         (.onSuccess out success))))))

(defn ss-handle
  [p f]
  (let [ss (SingleSubject/create)]
    (.subscribeWith
     p
     (reify SingleObserver
       (onSubscribe [_ _])
       (onError [_ err]
         (try
           (ss-flatten ss (f err))
           (catch Exception x
             (.onError ss x))))
       (onSuccess [_ success]
         (try
           (ss-flatten ss (f success nil))
           (catch Exception x
             (.onError ss x))))))
    ss))

(extend Single
  p/IPromise
  {:-then ss-then
   :-handle ss-handle})

(def singlesubject-factory (SingleSubjectPromiseFactory.))
