(ns laters.control.rwpromise-test
  (:require
   [laters.control.rwpromise :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
   [laters.abstract.monad-test :as m.t]
   [laters.monoid :as monoid])
  (:import
   [java.io Writer]))

(deftest RWPromise-test
  (testing "return"
    (is (= {:monad.writer/output nil
            :monad/val :foo}

           (-> (m/return sut/rwpromise-ctx :foo)
               (r/run)
               deref))))

  (testing "bind"
    (m/with-context sut/rwpromise-ctx
      (is (= {:monad.writer/output nil
              :monad/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run)
                 deref)))))

  (testing "bind-catch"
    (m/with-context sut/rwpromise-ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v] (throw (ex-info "boo" {:x 50}))))
                 (error/catch (fn [e]
                                (let [{x :x :as _d} (ex-data e)]
                                  (prn "catch-type" (type e) (ex-data e))
                                  ;; (prn "catch data" d)
                                  (m/return (inc x)))))
                 (r/run)
                 deref)))))

  (testing "run-catch"
    (m/with-context sut/rwpromise-ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwpromise-mv
                            sut/rwpromise-ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{x :x :as _d} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)
                 deref)))))

  ;; TODO - how to preserve writer output in catch etc

  )


;; put promise failures into a marker for comparisons
(deftype Failure [e]
  Object

  ;; for the purposes of these tests we define equals between
  ;; Failures as being identical? of causes of exceptions
  (equals [a b]
    (prn "equals" a b)
    (and
     (some? (ex-data e))
     (instance? Failure b)
     (identical? (sut/unwrap-cause e)
                 (sut/unwrap-cause (.-e b))))))

(defmethod print-method Failure [f ^Writer w]
  (let [e (.-e f)]
    (.write w "<< Failure: ")
    (.write w (prn-str e))
    (.write w " >>")))

(defn failure
  [e]
  (->Failure
   (sut/unwrap-exception e)))

(defmacro catch-failure
  [& body]
  `(try
     ~@body
     (catch Exception e#
       ;; we corall an error into being a marker value
       ;; for simple test flow - errors are now just values
       {:monad/val (failure e#)})))

(defn run-deref
  [mv arg]
  (catch-failure
   (deref
    (r/run mv arg))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[{a-val :monad/val}
         {b-val :monad/val}] (map #(run-deref % {:monad.reader/env :foo}) [mva mvb])]
    (is (= expected-val a-val))
    (is (= a-val b-val))))

(deftest monad-law-test
  (testing "bind"
    (testing "left-identity"
        (run-compare-vals
         (m.t/left-identity-test-mvs
          sut/rwpromise-ctx
          10
          (fn [v] (m/return sut/rwpromise-ctx (inc v))))
         11)
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/left-identity-test-mvs
            sut/rwpromise-ctx
            10
            (fn [_v] (error/reject sut/rwpromise-ctx x)))
           (failure x))))
      (testing "right-identity"
        (run-compare-vals
         (m.t/right-identity-test-mvs
          sut/rwpromise-ctx
          (sut/success-rwpromise-mv sut/rwpromise-ctx :foo))
         :foo)
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/right-identity-test-mvs
            sut/rwpromise-ctx
            (sut/failure-rwpromise-mv sut/rwpromise-ctx x))
           (failure x))))
      (testing "associativity"
        (run-compare-vals
         (m.t/associativity-test-mvs
          sut/rwpromise-ctx
          (sut/success-rwpromise-mv sut/rwpromise-ctx "foo")
          #(m/return sut/rwpromise-ctx (str % "bar"))
          #(m/return sut/rwpromise-ctx (str % "baz")))
         "foobarbaz")
        (let [x (ex-info "boo" {})]
          (run-compare-vals
           (m.t/associativity-test-mvs
            sut/rwpromise-ctx
            (sut/failure-rwpromise-mv sut/rwpromise-ctx x)
            #(m/return sut/rwpromise-ctx (str % "bar"))
            #(m/return sut/rwpromise-ctx (str % "baz")))
           (failure x)))))
  (testing "catch"
    (testing "plain value")
    (testing "failure"))
  (testing "finally"
    (testing "plain value")
    (testing "failure")))
