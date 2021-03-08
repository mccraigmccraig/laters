(ns promisefx.control.rws-promise-test
  (:require
   [promisefx.context :as ctx]
   [promisefx.control.rws-promise :as sut]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [promisefx.fx.monad :as m]
   [promisefx.data.exception :as data.ex]
   [promisefx.data.runnable :as r]
   [promisefx.fx.error :as error]
   [promisefx.fx.error-test :as err.t]
   [promisefx.fx.monad-test :as m.t])
  #?(:clj
     (:import
      [java.io Writer])))

(deftest rws-promise-ctx-test
  (testing "return"
    (is (= {:promisefx.writer/output nil
            :promisefx/val :foo}

           (-> (m/return sut/ctx :foo)
               (r/run)
               deref))))

  (testing "bind"
    (ctx/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 101}

             (-> (m/return 100)
                 (m/bind (fn [v] (m/return (inc v))))
                 (r/run)
                 deref)))))

  (testing "bind-catch"
    (ctx/with-context sut/ctx
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
    (ctx/with-context sut/ctx
      (is (= {:promisefx.writer/output nil
              :promisefx/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rws-promise-mv
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
(deftype RWSPromiseTestFailure [e]
  Object

  ;; for the purposes of these tests we define equals between
  ;; Failures as being identical? of causes of exceptions
  (equals [a b]
    ;; (prn "equals" a b)
    (and
     (some? (ex-data e))
     (instance? RWSPromiseTestFailure b)
     (and
      (identical? (sut/unwrap-cause e)
                  (sut/unwrap-cause (.-e b)))
      ;; should we be testing ex-data equality too ?
      ;; (= (ex-data e) (ex-data (.-e b)))
      ))))

(defmethod clojure.core/print-method RWSPromiseTestFailure [f #?(:clj ^Writer w :cljs w)]
  (let [e (.-e f)]
    (.write w "<< RWSPromiseTestFailure: ")
    (.write w (prn-str e))
    (.write w " >>")))

(defn failure
  [e]
  (->RWSPromiseTestFailure
   (data.ex/unwrap-exception e)))

(defmacro catch-failure
  [& body]
  `(try
     ~@body
     (catch Exception e#
       (sut/unwrap-failure-channel e#))))

(defn run-deref
  [mv arg]
  (catch-failure
   (deref
    (r/run mv arg))))

(defn run-compare-vals
  [[mva mvb] expected-val]
  (let [[a-val b-val] (map #(run-deref % {:promisefx.reader/env :foo}) [mva mvb])]
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
          [
           [10 (fn [v] (m/return (inc v)))
            {:promisefx.writer/output nil
             :promisefx/val  11}]
           [10 (fn [_v] (error/reject x))
            {:promisefx.writer/output nil
             :promisefx/err x}]
           ]

          :right-identity
          ;; ;; [mv expected-mv]
          [
           [(m/return :foo)
            {:promisefx.writer/output nil
             :promisefx/val :foo}]
           [(error/reject x)
            {:promisefx.writer/output nil
             :promisefx/err x}]
           ]

          :associativity
          ;; [mv f g expected-mv]
          [
           [(m/return "foo")
            #(m/return (str % "bar"))
            #(m/return (str % "baz"))
            {:promisefx.writer/output nil
             :promisefx/val "foobarbaz"} ]
           [(error/reject x)
            #(m/return (str % "bar"))
            #(m/return (str % "baz"))
            {:promisefx.writer/output nil
             :promisefx/err x}]
           ]
          })
        )

      (testing (str "catch/reject " (pr-str (ctx/get-tag ctx)))

        ;; (err.t/run-monad-law-tests
        ;;  sut/ctx
        ;;  run-compare-vals

        ;;  {:left-identity
        ;;   ;; [a mf expected-mv]
        ;;   [
        ;;    [x #(error/reject %) (failure x)]
        ;;    [x #(m/return %) x]
        ;;    ]

        ;;   :right-identity
        ;;   ;; [mv expected-mv]
        ;;   [
        ;;    [(error/reject x) (failure x)]
        ;;    [(m/return :foo) :foo]
        ;;    ]

        ;;   :associativity
        ;;   ;; [mv f g expected-mv]
        ;;   [
        ;;    [(error/reject x)
        ;;     #(error/reject %)
        ;;     #(error/reject %)
        ;;     (failure x)]
        ;;    [(error/reject sut/ctx x)
        ;;     #(m/return %)
        ;;     #(m/return %)
        ;;     x]
        ;;    ]})

        ))))

(deftest monad-law-tests

  (context-monad-law-tests sut/ctx)

  (context-monad-law-tests sut/tagged-ctx)

  (context-monad-law-tests sut/boxed-tagged-ctx)
  )
