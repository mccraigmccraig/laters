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
              sut/ctx
              (m/return sut/ctx 10)
              (fn [a]
                (m/bind
                 sut/ctx
                 (m/return sut/ctx 5)
                 (fn [b] (m/return sut/ctx (+ a b))))))]
      (is (= 15
             mv))))

  (testing "uncaught error"
    (let [mv (m/bind
              sut/ctx
              (m/return sut/ctx 10)
              (fn [a]
                (m/bind
                 sut/ctx
                 (throw (ex-info "boo!" {}))
                 (fn [b] (m/return sut/ctx (+ a b))))))]
      (is (s.f/failure? mv))
      (is (= "boo!" (some-> mv :e .getMessage)))))

  (testing "catch"
    (let [mv (error/catch
                 sut/ctx
                 (throw (ex-info "boo!" {}))
               (fn [e] (m/return sut/ctx 10)))]
      (is (= 10
             mv)))))

(deftest monad-law-test
  (testing "left-identity"
    (let [[a b :as mvs] (m.t/left-identity-test-mvs
                         sut/ctx
                         10
                         (fn [v] (m/return sut/ctx (inc v))))

          [a-val b-val] (map #(extractable.p/-extract %) mvs)]
      (is (= 11 a-val))
      (is (= a-val b-val))))
  (testing "right-identity"
    (let [[a b :as mvs] (m.t/right-identity-test-mvs
                         sut/ctx
                         :foo)

          [a-val b-val] (map #(extractable.p/-extract %) mvs)]
      (is (= :foo a-val))
      (is (= a-val b-val))))
  (testing "associativity"
    (let [[a b :as mvs] (m.t/associativity-test-mvs
                         sut/ctx
                         "foo"
                         #(m/return sut/ctx (str % "bar"))
                         #(m/return sut/ctx (str % "baz")))
          [a-val b-val] (map #(extractable.p/-extract %) mvs)]
      (is (= "foobarbaz" a-val))
      (is (= a-val b-val)))))
