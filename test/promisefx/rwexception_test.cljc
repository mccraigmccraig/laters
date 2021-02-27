(ns promisefx.control.rwexception-test
  (:require
   [promisefx.control.rwexception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.abstract.monad :as m]
   [promisefx.abstract.monad.protocols :as m.p]
   [promisefx.abstract.context.protocols :as ctx.p]
   [promisefx.abstract.runnable :as r]
   [promisefx.abstract.error :as error]
   [promisefx.abstract.monad-test :as m.t]
   [promisefx.control.either :as either]))

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
    (testing "left-identity"
        (run-compare-vals
         (m.t/left-identity-test-mvs
          sut/rwexception-ctx
          10
          (fn [v] (m/return sut/rwexception-ctx (inc v))))
         11)
        (let[x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/left-identity-test-mvs
            sut/rwexception-ctx
            10
            (fn [_v] (error/reject sut/rwexception-ctx x)))
           (sut/failure x))))
      (testing "right-identity"
        (run-compare-vals
         (m.t/right-identity-test-mvs
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx :foo))
         :foo)
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/right-identity-test-mvs
            sut/rwexception-ctx
            (sut/error-rwexception-val sut/rwexception-ctx x))
           (sut/failure x))))
      (testing "associativity"
        (run-compare-vals
         (m.t/associativity-test-mvs
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx "foo")
          #(m/return sut/rwexception-ctx (str % "bar"))
          #(m/return sut/rwexception-ctx (str % "baz")))
         "foobarbaz")
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/associativity-test-mvs
            sut/rwexception-ctx
            (sut/error-rwexception-val sut/rwexception-ctx x)
            #(m/return sut/rwexception-ctx (str % "bar"))
            #(m/return sut/rwexception-ctx (str % "baz")))
           (sut/failure x)))))

  (testing "catch"
    (testing "left-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/left-identity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          x
          #(error/reject' sut/rwexception-ctx %))
         (sut/failure x))
        (run-compare-vals
         (m.t/left-identity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          x
          #(m/return' sut/rwexception-ctx %))
         x)))
    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x))
         (sut/failure x))
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx :foo))
         :foo)))
    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x)
          (partial error/reject' sut/rwexception-ctx)
          (partial error/reject' sut/rwexception-ctx))
         (sut/failure x)))
      (let [x (ex-info "boo" {:foo 100})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/catch'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x)
          (partial m/return' sut/rwexception-ctx)
          (partial m/return' sut/rwexception-ctx))
         x))))

  (testing "finally"
    (testing "left-identity"
      (let [x (ex-info "boo" {:foo :bar})]

        ;; this is interesting ... seem to have
        ;; to match the return fn to either
        ;; m/return or error/reject depending
        ;; on whether we are following the plain
        ;; or error path... need to think about this

        (run-compare-vals
         (m.t/left-identity-test-mvs
          {:bind error/finally'
           :return error/reject'}
          sut/rwexception-ctx
          x
          #(error/reject' sut/rwexception-ctx %))
         (sut/failure x))


        (run-compare-vals
         (m.t/left-identity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/rwexception-ctx
          x
          #(m/return' sut/rwexception-ctx %))
         x)))

    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/finally'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x))
         (sut/failure x))
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/rwexception-ctx
          (sut/plain-rwexception-val sut/rwexception-ctx :foo))
         :foo)))

    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/finally'
           :return error/reject'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x)
          (partial error/reject' sut/rwexception-ctx)
          (partial error/reject' sut/rwexception-ctx))
         (sut/failure x)))
      (let [x (ex-info "boo" {:foo 100})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/rwexception-ctx
          (sut/error-rwexception-val sut/rwexception-ctx x)
          (partial m/return' sut/rwexception-ctx)
          (partial m/return' sut/rwexception-ctx))
         (sut/failure x))))
    )



  )
