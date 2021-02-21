(ns laters.control.prw-test
  (:require
   [laters.abstract.monad :as m]
   [laters.control.prw :as sut]
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])))

(defmacro left-identity-test
  [f x])

(t/deftest left-identity
  (let [f #(m/return sut/prw-ctx (* 2 %))
        x 100

        a (sut/prw-let
            [x' (m/return x)]
            (f x))
        a-r @(sut/run-prw a {})

        b (sut/prw-let
            (f x))
        b-r @(sut/run-prw b {})]

    (t/is (= a-r b-r))))

(t/deftest right-identity
  (let [m (m/return sut/prw-ctx 100)

        a (m/mlet sut/prw-ctx
            [x m]
            (m/return x))
        a-r @(sut/run-prw a {})

        b (m/mlet sut/prw-ctx
            m)
        b-r @(sut/run-prw b {})]
    (t/is (= a-r b-r))))

(t/deftest associativity
  (let [m (m/return sut/prw-ctx 100)
        f #(m/return sut/prw-ctx (* 2 %))
        g #(m/return sut/prw-ctx (- % 50))

        a (m/mlet sut/prw-ctx
            [y (m/mlet sut/prw-ctx
                 [x m]
                 (f x))]
            (g y))
        a-r @(sut/run-prw a {})

        b (m/mlet sut/prw-ctx
            [x m]
            (m/mlet sut/prw-ctx
              [y (f x)]
              (g y)))
        b-r @(sut/run-prw b {})

        c (m/mlet sut/prw-ctx
            [x m
             y (f x)]
            (g y))
        c-r @(sut/run-prw c {})]

    (t/is (= a-r b-r c-r))))
