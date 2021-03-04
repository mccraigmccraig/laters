(ns promisefx.control.exception-test
  (:require
   [promisefx.control.exception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.fx.monad :as m]
   [promisefx.fx.error :as error]
   [promisefx.data.extractable.protocols :as extractable.p]
   [promisefx.data.success-failure :as s.f]
   [promisefx.fx.monad-test :as m.t]))

(deftest Exception-test
  (testing "bind"
    (let [mv (m/bind
              sut/untagged-ctx
              (m/return sut/untagged-ctx 10)
              (fn [a]
                (m/bind
                 sut/untagged-ctx
                 (m/return sut/untagged-ctx 5)
                 (fn [b] (m/return sut/untagged-ctx (+ a b))))))]
      (is (= 15
             mv))))

  (testing "uncaught error"
    (let [mv (m/bind
              sut/untagged-ctx
              (m/return sut/untagged-ctx 10)
              (fn [a]
                (m/bind
                 sut/untagged-ctx
                 (throw (ex-info "boo!" {}))
                 (fn [b] (m/return sut/untagged-ctx (+ a b))))))]
      (is (s.f/failure? mv))
      (is (= "boo!" (some-> mv :e .getMessage)))))

  (testing "catch"
    (let [mv (error/catch
                 sut/untagged-ctx
                 (throw (ex-info "boo!" {}))
               (fn [e] (m/return sut/untagged-ctx 10)))]
      (is (= 10
             mv)))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (is (= expected-val mva))
  (is (= mva mvb)))

(deftest monad-law-test
  (m.t/run-monad-law-tests
   sut/untagged-ctx
   run-compare-vals

   {:left-identity
    [[10 (fn [v] (m/return sut/untagged-ctx (inc v))) 11]]

    :right-identity
    [[:foo :foo]]

    :associativity
    [["foo"
      #(m/return sut/untagged-ctx (str % "bar"))
      #(m/return sut/untagged-ctx (str % "baz"))
      "foobarbazx"]]}))
