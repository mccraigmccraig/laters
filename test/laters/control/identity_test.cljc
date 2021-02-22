(ns laters.control.identity-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [laters.abstract.monad :as m]
   [laters.abstract.monad-test :as m.t]
   [laters.control.identity :as sut]))

(deftest left-identity-test
  (let [[a b] (m.t/left-identity-mvs
               sut/identity-ctx
               10
               (fn [v] (m/return sut/identity-ctx (inc v))))]
       (is (= a b))))
