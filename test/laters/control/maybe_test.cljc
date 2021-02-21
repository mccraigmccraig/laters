(ns laters.control.maybe-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [laters.abstract.monad-test :as monad-test]
   [laters.control.maybe :as sut]))

(deftest maybe-context-monad-law-tests
  (testing "left-identity"
    (monad-test/left-identity-test sut/maybe-ctx))
  (testing "right-identity"
    (monad-test/right-identity-test sut/maybe-ctx))
  (testing "associativity"
    (monad-test/associativity-test sut/maybe-ctx)))

(deftest tagged-maybe-context-monad-law-tests
  (testing "left-identity"
    (monad-test/left-identity-test sut/tagged-maybe-ctx))
  (testing "right-identity"
    (monad-test/right-identity-test sut/tagged-maybe-ctx))
  (testing "associativity"
    (monad-test/associativity-test sut/tagged-maybe-ctx)))


(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.control.identity :as m.id])
  (require '[laters.control.maybe :as m.maybe])

  (m/mlet m.maybe/maybe-ctx
          [a (m/return 1)
           b (m.maybe/nothing)
           c (m/return 10)]
          (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
          [a (m/return 1)
           b (m/return 5)
           c (m/return 10)]
          (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
          [a (m/return 1)
           b (m/return 5)
           :when nil
           c (m/return 10)]
          (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
          [a (m/return 1)
           b (m/return 5)
           :when true
           c (m/return 10)]
          (m/return (+ a b c)))

  (m/mlet m.maybe/maybe-ctx
          [a (m/mlet m.id/identity-ctx [a (m/return 10)] (m/return a))
           b (m/return 3)]
          (m/return (* a b)))


  (m/mlet m.maybe/tagged-maybe-ctx
          [a (m/return 1)
           b (m/return 5)
           :when true
           c (m/return 10)]
          (m/return (+ a b c))))
