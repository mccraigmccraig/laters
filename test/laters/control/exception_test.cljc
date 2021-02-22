(ns laters.control.exception-test
  (:require
   [laters.control.exception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.error :as error]
   [laters.abstract.context.protocols :as ctx.p]
   [laters.control.either :as either]
   [laters.abstract.monad-test :as m.t]))

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

(deftest monad-law-test
  (testing "left-identity"
    (let [[a b :as mvs] (m.t/left-identity-test-mvs
                         sut/exception-ctx
                         10
                         (fn [v] (m/return sut/exception-ctx (inc v))))

          [a-val b-val] (map #(ctx.p/-extract %) mvs)]
      (is (= 11 a-val))
      (is (= a-val b-val))))
  (testing "right-identity"
    (let [[a b :as mvs] (m.t/right-identity-test-mvs
                         sut/exception-ctx
                         (sut/success sut/exception-ctx :foo))

          [a-val b-val] (map #(ctx.p/-extract %) mvs)]
      (is (= :foo a-val))
      (is (= a-val b-val))))
  (testing "associativity"
    (let [[a b :as mvs] (m.t/associativity-test-mvs
                         sut/exception-ctx
                         (sut/success sut/exception-ctx "foo")
                         #(m/return sut/exception-ctx (str % "bar"))
                         #(m/return sut/exception-ctx (str % "baz")))
          [a-val b-val] (map #(ctx.p/-extract %) mvs)]
      (is (= "foobarbaz" a-val))
      (is (= a-val b-val)))))
