(ns promisefx.control.promise-test
  (:require
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :include-macros true :refer-macros [deftest testing is]])
   [promisefx.control.promise :as sut]
   [promisefx.data.exception :as data.ex]
   [promisefx.data.runnable :as r]
   [promisefx.fx.monad :as m]
   [promisefx.fx.error :as error]
   [promisefx.fx.error-test :as err.t]
   [promisefx.fx.monad-test :as m.t])
  #?(:clj
     (:import
      [java.io Writer])))

(deftype PromiseTestFailure [e]
  Object
  (equals [a b]
    (and
     (some? (ex-data e))
     (instance? PromiseTestFailure b)
     (and
      (identical? (data.ex/unwrap-exception e)
                  (data.ex/unwrap-exception (.-e b)))))))

(defmethod clojure.core/print-method PromiseTestFailure [f #?(:clj ^Writer w :cljs w)]
  (let [e (.-e f)]
    (.write w "<< RWSPromiseTestFailure: ")
    (.write w (prn-str e))
    (.write w " >>")))

(defn failure
  [e]
  (->PromiseTestFailure
   (data.ex/unwrap-exception e)))

(defmacro catch-failure
  [& body]
  `(try
     ~@body
     (catch Exception e#
       ;; we corall an error into being a marker value
       ;; for simple test flow - errors are now just values
       (failure e#))))

(defn catch-deref
  [mv]
  (catch-failure
   (deref mv)))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[a-val b-val] (map #(catch-deref %) [mva mvb])]
    (is (= expected-val a-val))
    (is (= a-val b-val))))

(deftest monad-law-test
  (testing "bind"
    (testing "left-identity"
      (doseq [[a mf xv] [[10 (fn [v] (m/return sut/ctx (inc v))) 11]
                         (let [x (ex-info "boo" {})]
                           [10 (fn [_v] (error/reject sut/ctx x)) (failure x) ])]]
        (m.t/run-left-identity-test sut/ctx run-compare-vals a mf xv)))
    (testing "right-identity"
      (doseq [[mv xv] [[(m/return sut/ctx :foo) :foo]
                       (let [x (ex-info "boo" {})]
                         [(error/reject sut/ctx x)
                          (failure x)])]]
        (m.t/run-right-identity-test sut/ctx run-compare-vals mv xv)))
    (testing "associativity"
      (doseq [[m f g xv] [[(m/return sut/ctx "foo")
                           #(m/return sut/ctx (str % "bar"))
                           #(m/return sut/ctx (str % "baz"))
                           "foobarbaz" ]
                          (let [x (ex-info "boo" {})]
                            [(error/reject sut/ctx x)
                             #(m/return sut/ctx (str % "bar"))
                             #(m/return sut/ctx (str % "baz"))
                             (failure x)])]]
        (m.t/run-associativity-test sut/ctx run-compare-vals m f g xv))))

  (testing "catch"
    (testing "left-identity"
      (doseq [[a mf xv] (let [x (ex-info "boo" {})]
                          [[x #(error/reject' sut/ctx %) (failure x)]
                           [x #(m/return' sut/ctx %) x]])]
        (err.t/run-left-identity-test sut/ctx run-compare-vals a mf xv)))

    (testing "right-identity"
      (doseq [[mv xv] (let [x (ex-info "boo" {})]
                        [[(error/reject sut/ctx x) (failure x)]
                         [(m/return sut/ctx :foo) :foo]])]
        (err.t/run-right-identity-test sut/ctx run-compare-vals mv xv)))

    (testing "associativity"
      (doseq [[m f g xv] (let [x (ex-info "boo" {})]
                           [[(error/reject sut/ctx x)
                             (partial error/reject' sut/ctx)
                             (partial error/reject' sut/ctx)
                             (failure x)]
                            [(error/reject sut/ctx x)
                             (partial m/return' sut/ctx)
                             (partial m/return' sut/ctx)
                             x]])]
        (err.t/run-associativity-test sut/ctx run-compare-vals m f g xv)))

    ))
