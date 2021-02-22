(ns laters.control.rwexception-test
  (:require
   [laters.control.rwexception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.context.protocols :as ctx.p]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
   [laters.abstract.monad-test :as m.t]
   [laters.abstract.error-test :as error.t]
   [laters.control.either :as either]))

(deftest RWException-test
  (testing "return"
    (is (= {:monad.writer/output nil
            :monad/val :foo}

           (-> (m/return sut/rwexception-ctx :foo)
               (r/run)))))
  (testing "bind"
    (m/with-context sut/rwexception-ctx
      (is (= {:monad.writer/output nil
              :monad/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run))))))
  (testing "bind-catch"
    (m/with-context sut/rwexception-ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v] (throw (ex-info "boo" {:x 50}))))
                 (error/catch (fn [e] (let [{x :x} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run))))))
  (testing "run-catch"
    (m/with-context sut/rwexception-ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwexception-val
                            sut/rwexception-ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{x :x} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)))))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[{a-val :monad/val}
         {b-val :monad/val}] (map #(r/run % {:monad.reader/env :foo}) [mva mvb])]
    (is (= expected-val a-val))
    (is (= a-val b-val))))

(deftest monad-law-test
  (testing "bind"
    (testing "plain value"
      (testing "left-identity"
        (run-compare-vals
         (m.t/left-identity-test-mvs
          sut/rwexception-ctx
          10
          (fn [v] (m/return sut/rwexception-ctx (inc v))))
         11))
      (testing "right-identity"
        (run-compare-vals
         (m.t/right-identity-test-mvs
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx :foo))
         :foo))
      (testing "associativity"
        (run-compare-vals
         (m.t/associativity-test-mvs
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx "foo")
          #(m/return sut/rwexception-ctx (str % "bar"))
          #(m/return sut/rwexception-ctx (str % "baz")))
         "foobarbaz")))

    (testing "failure"
      (testing "left-identity"
        (let[x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/left-identity-test-mvs
            sut/rwexception-ctx
            10
            (fn [_v] (error/reject sut/rwexception-ctx x)))
           (sut/failure x))))
      (testing "right-identity"
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/right-identity-test-mvs
            sut/rwexception-ctx
            (sut/error-rwexception-val sut/rwexception-ctx x))
           (sut/failure x))))
      (testing "associativity"
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/associativity-test-mvs
            sut/rwexception-ctx
            (sut/error-rwexception-val sut/rwexception-ctx x)
            #(m/return sut/rwexception-ctx (str % "bar"))
            #(m/return sut/rwexception-ctx (str % "baz")))
           (sut/failure x))))))

  (testing "catch"
    (testing "left-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (error.t/left-identity-test-mvs
          sut/rwexception-ctx
          x
          #(error/reject' sut/rwexception-ctx %))
         (sut/failure x))
        (run-compare-vals
         (error.t/left-identity-test-mvs
          sut/rwexception-ctx
          x
          #(m/return' sut/rwexception-ctx %))
         x)))
    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (error.t/right-identity-test-mvs
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x))
         (sut/failure x))))
    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (error.t/associativity-test-mvs
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x)
          (partial error/reject' sut/rwexception-ctx)
          (partial error/reject' sut/rwexception-ctx))
         (sut/failure x)))))

  (testing "finally")



  )
