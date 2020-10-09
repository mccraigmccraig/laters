(ns laters.concurrency.promise.rxjava
  (:require
   [laters.concurrency.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [io.reactivex.subjects SingleSubject]
   [io.reactivex Single SingleObserver]))

(deftype SingleSubjectPromiseImpl [scheduler]
  p/IPromiseImpl
  (-type [_]
    [::SingleSubjectPromise])
  (-resolved [_ v]
    (Single/just v))
  (-rejected [_ err]
    (Single/error err))
  (-deferred [_]
    (SingleSubject/create)))

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

(defn ss-then
  ([p f] (ss-then p f nil))
  ([p f ^SingleSubjectPromiseImpl impl]
   (let [ss (SingleSubject/create)]
     (when-let [scheduler (some-> impl .scheduler)]
       (.observeOn ss scheduler))
     (.subscribeWith
      p
      (reify SingleObserver
        (onSubscribe [_ _])
        (onError [_ err]
          (.onError ss err))
        (onSuccess [_ success]
          (try
            (ss-flatten ss (f success))
            (catch Exception x
              (.onError ss x))))))
     ss)))

(defn ss-handle
  ([p f] (ss-handle p f nil))
  ([p f ^SingleSubjectPromiseImpl impl]
   (let [ss (SingleSubject/create)]
     (when-let [scheduler (some-> impl .scheduler)]
       (.observeOn ss scheduler))
     (.subscribeWith
      p
      (reify SingleObserver
        (onSubscribe [_ _])
        (onError [_ err]
          (try
            (ss-flatten ss (f nil err))
            (catch Exception x
              (.onError ss x))))
        (onSuccess [_ success]
          (try
            (ss-flatten ss (f success nil))
            (catch Exception x
              (.onError ss x))))))
     ss)))

(defn ss-resolve!
  [^SingleSubject p v]
  (.onSuccess p v))

(defn ss-reject!
  [^SingleSubject p ^Throwable err]
  (.onError p err))

(defn ss-deref
  [p]
  (let [pp (promesa/deferred)]
    (.subscribeWith
     p
     (reify SingleObserver
       (onSubscribe [_ _])
       (onError [_ err]
         (promesa/reject! pp err))
       (onSuccess [_ success]
         (promesa/resolve! pp success))))

    (deref pp)))

(extend Single
  p/IPromise
  {:-then ss-then
   :-handle ss-handle
   :-resolve! ss-resolve!
   :-reject! ss-reject!
   :-deref ss-deref})

(defn make-single-subject-promise-impl
  [scheduler]
  (SingleSubjectPromiseImpl. scheduler))

(def default-impl (make-single-subject-promise-impl nil))
