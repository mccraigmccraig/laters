(ns promisefx.control.prws-test
  (:require
   [promisefx.control.prws :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.fx.monad :as m]
   [promisefx.data.runnable :as r]
   [promisefx.fx.error :as error]
   [promisefx.fx.error-test :as err.t]
   [promisefx.fx.monad-test :as m.t])
  #?(:clj
     (:import
      [java.io Writer])))

(deftest PRWS-ctx-test
  (testing "return"
    (is (= {:promisefx.writer/output nil
            :promisefx/val :foo}

           (-> (m/return sut/ctx :foo)
               (r/run)
               deref))))

  (testing "bind"
    (m/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run)
                 deref)))))

  (testing "bind-catch"
    (m/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v] (throw (ex-info "boo" {:x 50}))))
                 (error/catch (fn [e]
                                (let [{x :x :as _d} (ex-data e)]
                                  ;; (prn "catch-type" (type e) (ex-data e))
                                  ;; (prn "catch data" d)
                                  (m/return (inc x)))))
                 (r/run)
                 deref)))))

  (testing "run-catch"
    (m/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwpromise-mv
                            sut/ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{x :x :as _d} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)
                 deref)))))

  ;; TODO - how to preserve writer output in catch etc

  )


;; put promise failures into a marker for comparisons
(deftype PRWSFailure [e]
  Object

  ;; for the purposes of these tests we define equals between
  ;; Failures as being identical? of causes of exceptions
  (equals [a b]
    ;; (prn "equals" a b)
    (and
     (some? (ex-data e))
     (instance? PRWSFailure b)
     (and
      (identical? (sut/unwrap-cause e)
                  (sut/unwrap-cause (.-e b)))
      ;; should we be testing ex-data equality too ?
      ;; (= (ex-data e) (ex-data (.-e b)))
      ))))

(defmethod clojure.core/print-method PRWSFailure [f #?(:clj ^Writer) w]
  (let [e (.-e f)]
    (.write w "<< PRWSFailure: ")
    (.write w (prn-str e))
    (.write w " >>")))

(defn failure
  [e]
  (->PRWSFailure
   (sut/unwrap-exception e)))

(defmacro catch-failure
  [& body]
  `(try
     ~@body
     (catch Exception e#
       ;; we corall an error into being a marker value
       ;; for simple test flow - errors are now just values
       {:promisefx/val (failure e#)})))

(defn run-deref
  [mv arg]
  (catch-failure
   (deref
    (r/run mv arg))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[{a-val :promisefx/val}
         {b-val :promisefx/val}] (map #(run-deref % {:promisefx.reader/env :foo}) [mva mvb])]
    (is (= expected-val a-val))
    (is (= a-val b-val))))

(deftest monad-law-test
  (testing "bind"
    (testing "left-identity"

      (doseq [[a mf xv] [[10 (fn [v] (m/return sut/ctx (inc v))) 11]
                         (let [x (ex-info "boo" {})]
                           [10 (fn [_v] (error/reject sut/ctx x)) (failure x) ])]]
        (m.t/run-left-identity-test sut/ctx run-compare-vals a mf xv)))

    (testing "right-identity"
      (doseq [[mv xv] [[(sut/success-rwpromise-mv sut/ctx :foo) :foo]
                       (let [x (ex-info "boo" {})]
                         [(sut/failure-rwpromise-mv sut/ctx x)
                          (failure x)])]]
        (m.t/run-right-identity-test sut/ctx run-compare-vals mv xv)))

    (testing "associativity"
      (doseq [[m f g xv] [[(sut/success-rwpromise-mv sut/ctx "foo")
                           #(m/return sut/ctx (str % "bar"))
                           #(m/return sut/ctx (str % "baz"))
                           "foobarbaz" ]
                          (let [x (ex-info "boo" {})]
                            [(sut/failure-rwpromise-mv sut/ctx x)
                             #(m/return sut/ctx (str % "bar"))
                             #(m/return sut/ctx (str % "baz"))
                             (failure x)])]]
        (m.t/run-associativity-test sut/ctx run-compare-vals m f g xv))))

  (testing "catch"
    (testing "left-identity"
      (doseq [[a mf xv] (let [x (ex-info "boo" {})]
                          [[x #(error/reject' sut/ctx %) (failure x)]
                           [x #(m/return' sut/ctx %) x]])]
        (err.t/run-left-identity-test sut/ctx run-compare-vals a mf xv)))

    (testing "right-identity"
      (doseq [[mv xv] (let [x (ex-info "boo" {})]
                        [[(sut/failure-rwpromise-mv sut/ctx x) (failure x)]
                         [(sut/success-rwpromise-mv sut/ctx :foo) :foo]])]
        (err.t/run-right-identity-test sut/ctx run-compare-vals mv xv)))

    (testing "associativity"
      (doseq [[m f g xv] (let [x (ex-info "boo" {})]
                           [[(sut/failure-rwpromise-mv sut/ctx x)
                             (partial error/reject' sut/ctx)
                             (partial error/reject' sut/ctx)
                             (failure x)]
                            [(sut/failure-rwpromise-mv sut/ctx x)
                             (partial m/return' sut/ctx)
                             (partial m/return' sut/ctx)
                             x]])]
        (err.t/run-associativity-test sut/ctx run-compare-vals m f g xv))))
  (testing "finally"
    ))
