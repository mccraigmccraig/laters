(ns laters.control.identity-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [laters.abstract.monad :as m]
   [laters.abstract.tagged :as tagged]
   [laters.abstract.monad-test :as monad-test]
   [laters.control.identity :as sut]))

(deftest identity-context-monad-law-tests
  (testing "left-identity"
    (monad-test/left-identity-test 
     sut/identity-ctx
     10
     #(m/return sut/identity-ctx (* 2 %))))
  (testing "right-identity"
    (monad-test/right-identity-test sut/identity-ctxx))
  (testing "associativity"
    (monad-test/associativity-test sut/identity-ctx)))

(deftest tagged-identity-context-monad-law-tests
  (testing "left-identity"
    (monad-test/left-identity-test 
     sut/tagged-identity-ctx
     10
     #(m/return sut/tagged-identity-ctx (* 2 %))))
  (testing "right-identity"
    (monad-test/right-identity-test sut/tagged-identity-ctx))
  (testing "associativity"
    (monad-test/associativity-test sut/tagged-identity-ctx)))

(deftest identity-context-test
  (let [r (m/mlet sut/identity-ctx
                  [a (m/return 2)
                   b (m/return 7)]
                  (m/return (* a b)))]
    (is (= 14 r))))

(deftest tagged-identity-context-test
  (let [r (m/mlet sut/tagged-identity-ctx
                [a (m/return 2)
                 b (m/return 7)]
                (m/return (* a b)))]
    (is (= 14 (tagged/untag r)))))