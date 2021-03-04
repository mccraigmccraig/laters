(ns promisefx.fx.error-test
  (:require
   [promisefx.fx.monad :as m]
   [promisefx.fx.monad-test :as m.test]
   [promisefx.fx.error :as err]))

(defn run-left-identity-test
  [ctx run-compare-fn a mf expected-val]
  (m.test/run-left-identity-test
   {:bind err/catch'
    :return err/reject'}
   ctx
   run-compare-fn
   a
   mf
   expected-val))

(defn run-right-identity-test
  [ctx run-compare-fn mv expected-val]
  (m.test/run-right-identity-test
   {:bind err/catch'
    :return err/reject'}
   ctx
   run-compare-fn
   mv
   expected-val))

(defn run-associativity-test
  [ctx run-compare-fn m f g expected-val]
  (m.test/run-associativity-test
   {:bind err/catch'
    :return err/reject'}
   ctx
   run-compare-fn
   m
   f
   g
   expected-val))

(defn run-monad-law-tests
  [ctx run-compare-fn law-testdata]
  (m.test/run-monad-law-tests
   {:bind err/catch'
    :return err/reject'}
   ctx
   run-compare-fn
   law-testdata))

(defn run-both-left-identity-tests
  [ctx run-compare-fn a mf expected-val]
  (run-left-identity-test ctx run-compare-fn a mf expected-val)
  (m.test/run-left-identity-test ctx run-compare-fn a mf expected-val))

(defn run-both-right-identity-tests
  [ctx run-compare-fn mv expected-val]
  (run-right-identity-test ctx run-compare-fn mv expected-val)
  (m.test/run-right-identity-test ctx run-compare-fn mv expected-val))

(defn run-both-associativity-tests
  [ctx run-compare-fn m f g expected-val]
  (run-associativity-test ctx run-compare-fn m f g expected-val)
  (m.test/run-associativity-test ctx run-compare-fn m f g expected-val))
