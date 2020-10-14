(ns laters.concurrency.promise.vertx
  (:require
   [laters.concurrency.promise.protocols :as p]
   [promesa.core :as promesa])
  (:import
   [io.vertx.core Future Promise]
   [java.util.function Function]))

(defprotocol IVertxFutureSource
  (-future [this]))

(extend Future
  IVertxFutureSource
  {:-future (fn -future [this] this)})

(extend Promise
  IVertxFutureSource
  {:-future (fn -future [this] (.future this))})

(deftype VertxPromiseImpl []
  p/IPromiseImpl
  (-type [ctx]
    [:VertxPromise])
  (-resolved [_ v]
    (Future/succeededFuture v))
  (-rejected [_ err]
    (Future/failedFuture err))
  (-deferred [_]
    (.future (Promise/promise))))

(defn vertx-then
  ([p f] (vertx-then p f nil))
  ([p f ^VertxPromiseImpl impl]
   (.compose
    (-future p)
    (reify Function
      (apply [_ v]
        (let [r (f v)]
          (if (instance? Future r)
            r
            (Future/succeededFuture r))))))))

(defn vertx-handle
  ([p f] (vertx-handle p f nil))
  ([p f ^VertxPromiseImpl impl]
   (.compose
    (-future p)
    (reify Function
      (apply [_ v]
        (let [r (f v nil)]
          (if (instance? Future r)
            r
            (Future/succeededFuture r)))))
    (reify Function
      (apply [_ err]
        (let [r (f nil err)]
          (if (instance? Future r)
            r
            (Future/succeededFuture r))))))))

(defn vertx-resolve!
  [^Promise p v]
  (.complete p v))

(defn vertx-reject!
  [^Promise p ^Throwable err]
  (.fail p err))

(defn vertx-deref
  [p]
  (let [pp (promesa/deferred)]
    (vertx-handle
     p
     (fn [success error]
       (if (some? error)
         (promesa/reject! pp error)
         (promesa/resolve! pp success))))
    (deref pp)))

(extend Future
  p/IPromise
  {:-then vertx-then
   :-handle vertx-handle
   :-resolve! vertx-resolve!
   :-reject! vertx-reject!
   :-deref vertx-deref})

(extend Promise
  p/IPromise
  {:-then vertx-then
   :-handle vertx-handle
   :-resolve! vertx-resolve!
   :-reject! vertx-reject!
   :-deref vertx-deref})

(defn make-vertx-promise-impl
  []
  (VertxPromiseImpl.))

(def default-impl (make-vertx-promise-impl))
