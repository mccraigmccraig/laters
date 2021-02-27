(ns promisefx.control.identity-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [promisefx.abstract.monad :as m]
   [promisefx.abstract.monad-test :as m.t]
   [promisefx.control.identity :as sut]))

(deftest monad-law-tests
  (testing "left-identity"
    (let [[a b] (m.t/left-identity-test-mvs
                 sut/identity-ctx
                 10
                 (fn [v] (m/return sut/identity-ctx (inc v))))]
      (is (= 11 a))
      (is (= a b))))
  (testing "right-identity"
    (let [[a b] (m.t/right-identity-test-mvs
                 sut/identity-ctx
                 :foo)]
      (is (= :foo a))
      (is (= a b))))
  (testing "associativity"
    (let [[a b] (m.t/associativity-test-mvs
                 sut/identity-ctx
                 "foo"
                 #(m/return sut/identity-ctx (str % "bar"))
                 #(m/return sut/identity-ctx (str % "baz")))]
      (is (= "foobarbaz" a))
      (is (= a b)))))
