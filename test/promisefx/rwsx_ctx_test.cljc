(ns promisefx.fx.rws-ctx-test
  (:require
   [promisefx.rwsx-ctx :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.fx.monad :as m]
   [promisefx.fx.monad.protocols :as m.p]
   [promisefx.context.protocols :as ctx.p]
   [promisefx.data.runnable :as r]
   [promisefx.fx.error :as error]
   [promisefx.fx.monad-test :as m.t]))

(deftest RWException-test
  (testing "return"
    (is (= {:monad.writer/output nil
            :monad/val :foo}

           (-> (m/return sut/ctx :foo)
               (r/run)))))
  (testing "bind"
    (m/with-context sut/ctx
      (is (= {:monad.writer/output nil
              :monad/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run))))))
  (testing "bind-catch"
    (m/with-context sut/ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v] (throw (ex-info "boo" {:x 50}))))
                 (error/catch (fn [e] (let [{x :x} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run))))))
  (testing "run-catch"
    (m/with-context sut/ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwexception-val
                            sut/ctx
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
      (m.t/run-left-identity-test
       sut/ctx
       run-compare-vals
       10
       (fn [v] (m/return sut/ctx (inc v)))
       11)
      (let[x (ex-info "boo" {})]
        (m.t/run-left-identity-test
         sut/ctx
         run-compare-vals
         10
         (fn [_v] (error/reject sut/ctx x))
         (sut/failure x))))

    (testing "right-identity"
      (m.t/run-right-identity-test
       sut/ctx
       run-compare-vals
       (sut/plain-rwexception-val sut/ctx :foo)
       :foo)
      (let [x (ex-info "boo" {})]
        (m.t/run-right-identity-test
         sut/ctx
         run-compare-vals
         (sut/error-rwexception-val sut/ctx x)
         (sut/failure x))))

    (testing "associativity"
      (m.t/run-associativity-test
       sut/ctx
       run-compare-vals
       (sut/plain-rwexception-val sut/ctx "foo")
       #(m/return sut/ctx (str % "bar"))
       #(m/return sut/ctx (str % "baz"))
       "foobarbaz")

      (let [x (ex-info "boo" {})]
        (m.t/run-associativity-test
         sut/ctx
         run-compare-vals
         (sut/error-rwexception-val sut/ctx x)
         #(m/return sut/ctx (str % "bar"))
         #(m/return sut/ctx (str % "baz"))
         (sut/failure x)))))

  (testing "catch"
    (testing "left-identity"
      (let [x (ex-info "boo" {})]
        (m.t/run-left-identity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         x
         #(error/reject' sut/ctx %)
         (sut/failure x))
        (m.t/run-left-identity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         x
         #(m/return' sut/ctx %)
         x)))

    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (m.t/run-right-identity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/error-rwexception-val sut/ctx x)
         (sut/failure x))
        (m.t/run-right-identity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/plain-rwexception-val sut/ctx :foo)
         :foo)))

    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (m.t/run-associativity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/error-rwexception-val sut/ctx x)
         (partial error/reject' sut/ctx)
         (partial error/reject' sut/ctx)
         (sut/failure x)))
      (let [x (ex-info "boo" {:foo 100})]
        (m.t/run-associativity-test
         {:bind error/catch'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/error-rwexception-val sut/ctx x)
         (partial m/return' sut/ctx)
         (partial m/return' sut/ctx)
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
          sut/ctx
          x
          #(error/reject' sut/ctx %))
         (sut/failure x))


        (run-compare-vals
         (m.t/left-identity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/ctx
          x
          #(m/return' sut/ctx %))
         x)))

    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/finally'
           :return error/reject'}
          sut/ctx
          (sut/error-rwexception-val sut/ctx x))
         (sut/failure x))
        (run-compare-vals
         (m.t/right-identity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/ctx
          (sut/plain-rwexception-val sut/ctx :foo))
         :foo)))

    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/finally'
           :return error/reject'}
          sut/ctx
          (sut/error-rwexception-val sut/ctx x)
          (partial error/reject' sut/ctx)
          (partial error/reject' sut/ctx))
         (sut/failure x)))
      (let [x (ex-info "boo" {:foo 100})]
        (run-compare-vals
         (m.t/associativity-test-mvs
          {:bind error/finally'
           :return m/return'}
          sut/ctx
          (sut/error-rwexception-val sut/ctx x)
          (partial m/return' sut/ctx)
          (partial m/return' sut/ctx))
         (sut/failure x))))
    )



  )
