(ns promisefx.control.rwsx-test
  (:require
   [promisefx.context :as ctx]
   [promisefx.control.rwsx :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.fx.monad :as m]
   [promisefx.data.runnable :as r]
   [promisefx.data.success-failure :as s.f]
   [promisefx.fx.error :as error]
   [promisefx.fx.monad-test :as m.t]
   [promisefx.fx.error-test :as err.t]))

(deftest RWSX-test
  (testing "return"
    (is (= {:promisefx.writer/output nil
            :promisefx/val :foo}

           (-> (m/return sut/ctx :foo)
               (r/run)))))
  (testing "bind"
    (ctx/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run))))))
  (testing "bind-catch"
    (ctx/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v] (throw (ex-info "boo" {:x 50}))))
                 (error/catch (fn [e] (let [{x :x} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run))))))
  (testing "run-catch"
    (ctx/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwsx-val
                            sut/ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{x :x} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)))))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[{a-val :promisefx/val}
         {b-val :promisefx/val}] (map #(r/run % {:promisefx.reader/env :foo}) [mva mvb])]
    (is (= expected-val a-val))
    (is (= a-val b-val))))

(defn context-monad-law-tests
  [ctx]
  (ctx/with-context ctx
    (let [x (ex-info "boo" {})]

      (testing (str "bind/return " (pr-str (ctx/get-tag ctx)))
        (m.t/run-monad-law-tests
         sut/ctx
         run-compare-vals

         {:left-identity
          ;; [a mf expected-mv]
          [[10
            (fn [v] (m/return (inc v)))
            11]
           [10
            (fn [_v] (error/reject x))
            (s.f/failure sut/ctx x)]]

          :right-identity
          ;; [mv expected-mv]
          [[(m/return :foo) :foo]
           [(error/reject x) (s.f/failure sut/ctx x)]]

          :associativity
          ;; [mv f g expected-mv]
          [[(m/return "foo")
            #(m/return (str % "bar"))
            #(m/return (str % "baz"))
            "foobarbaz"]
           [(error/reject x)
            #(m/return (str % "bar"))
            #(m/return (str % "baz"))
            (s.f/failure sut/ctx x)]]}))

      (testing (str "catch/reject " (pr-str (ctx/get-tag ctx)))

        (err.t/run-monad-law-tests
         sut/ctx
         run-compare-vals

         {:left-identity
          ;; [a mf expected-mv]
          [[x #(error/reject %) (s.f/failure sut/ctx x)]
           [x #(m/return %) x]]

          :right-identity
          ;; [mv expected-mv]
          [[(error/reject x) (s.f/failure sut/ctx x)]
           [(m/return :foo) :foo]]

          :associativity
          ;; [mv f g expected-mv]
          [[(error/reject x)
            #(error/reject %)
            #(error/reject %)
            (s.f/failure sut/ctx x)]
           [(error/reject x)
            #(m/return %)
            #(m/return %)
            x]]})))))


(deftest monad-law-test
  (context-monad-law-tests sut/ctx)


  (testing "finally"
    (testing "left-identity"
      (let [x (ex-info "boo" {:foo :bar})]

        ;; this is interesting ... seem to have
        ;; to match the return fn to either
        ;; m/return or error/reject depending
        ;; on whether we are following the plain
        ;; or error path... need to think about this


        (m.t/run-left-identity-test
         {:bind error/finally'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         x
         #(error/reject' sut/ctx %)
         (s.f/failure sut/ctx x))


        (m.t/run-left-identity-test
         {:bind error/finally'
          :return m/return'}
         sut/ctx
         run-compare-vals
         x
         #(m/return' sut/ctx %)
         x)))

    (testing "right-identity"
      (let [x (ex-info "boo" {})]
        (m.t/run-right-identity-test
         {:bind error/finally'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/failure-rwsx-val sut/ctx x)
         (s.f/failure sut/ctx x))
        (m.t/run-right-identity-test
         {:bind error/finally'
          :return m/return'}
         sut/ctx
         run-compare-vals
         (sut/success-rwsx-val sut/ctx :foo)
         :foo)))

    (testing "associativity"
      (let [x (ex-info "boo" {})]
        (m.t/run-associativity-test
         {:bind error/finally'
          :return error/reject'}
         sut/ctx
         run-compare-vals
         (sut/failure-rwsx-val sut/ctx x)
         (partial error/reject' sut/ctx)
         (partial error/reject' sut/ctx)
         (s.f/failure sut/ctx x)))
      (let [x (ex-info "boo" {:foo 100})]
        (m.t/run-associativity-test
         {:bind error/finally'
          :return m/return'}
         sut/ctx
         run-compare-vals
         (sut/failure-rwsx-val sut/ctx x)
         (partial m/return' sut/ctx)
         (partial m/return' sut/ctx)
         (s.f/failure sut/ctx x))))
    )



  )
