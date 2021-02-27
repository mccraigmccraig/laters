(ns promisefx.control.identity-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [promisefx.fx.monad :as m]
   [promisefx.fx.monad-test :as m.t]
   [promisefx.identity-ctx :as sut]))

(deftest monad-law-tests
  (testing "left-identity"
    (let [[a b] (m.t/left-identity-test-mvs
                 sut/ctx
                 10
                 (fn [v] (m/return sut/ctx (inc v))))]
      (is (= 11 a))
      (is (= a b))))
  (testing "right-identity"
    (let [[a b] (m.t/right-identity-test-mvs
                 sut/ctx
                 :foo)]
      (is (= :foo a))
      (is (= a b))))
  (testing "associativity"
    (let [[a b] (m.t/associativity-test-mvs
                 sut/ctx
                 "foo"
                 #(m/return sut/ctx (str % "bar"))
                 #(m/return sut/ctx (str % "baz")))]
      (is (= "foobarbaz" a))
      (is (= a b)))))
