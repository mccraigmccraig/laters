(ns laters.control.exception-test
  (:require
   [laters.control.exception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.error :as error]
   [laters.control.either :as either]))

(deftest Exception-test
  (testing "bind"
    (let [mv (m/bind
              sut/exception-ctx
              (m/return sut/exception-ctx 10)
              (fn [a]
                (m/bind
                 sut/exception-ctx
                 (m/return sut/exception-ctx 5)
                 (fn [b] (m/return sut/exception-ctx (+ a b))))))]
      (is (= (sut/success sut/exception-ctx 15)
             mv))))

  (testing "uncaught error"
    (let [mv (m/bind
              sut/exception-ctx
              (m/return sut/exception-ctx 10)
              (fn [a]
                (m/bind
                 sut/exception-ctx
                 (throw (ex-info "boo!" {}))
                 (fn [b] (m/return sut/exception-ctx (+ a b))))))]
      (is (sut/failure? mv))
      (is (= "boo!" (some-> mv :e .getMessage)))))

  (testing "catch"
    (let [mv (error/catch
                 sut/exception-ctx
                 (throw (ex-info "boo!" {}))
               (fn [e] (m/return sut/exception-ctx 10)))]
      (is (= (sut/success sut/exception-ctx 10)
             mv)))))

(deftest TaggedException-test
  (testing "bind"
    (let [mv (m/bind
              sut/tagged-exception-ctx
              (m/return sut/tagged-exception-ctx 10)
              (fn [a]
                (m/bind
                 sut/tagged-exception-ctx
                 (m/return sut/tagged-exception-ctx 5)
                 (fn [b] (m/return sut/tagged-exception-ctx (+ a b))))))]
      (is (= (sut/success sut/tagged-exception-ctx 15)
             mv))))

  (testing "uncaught error"
    (let [mv (m/bind
              sut/tagged-exception-ctx
              (m/return sut/tagged-exception-ctx 10)
              (fn [a]
                (m/bind
                 sut/tagged-exception-ctx
                 (throw (ex-info "boo!" {}))
                 (fn [b] (m/return sut/tagged-exception-ctx (+ a b))))))]
      (is (sut/failure? mv))
      (is (= "boo!" (-> mv :e .getMessage)))))

  (testing "catch"
    (let [mv (error/catch
                 sut/tagged-exception-ctx
                 (throw (ex-info "boo!" {}))
               (fn [_] (m/return sut/tagged-exception-ctx 10)))]
      (is (= (sut/success sut/tagged-exception-ctx 10)
             mv)))))
