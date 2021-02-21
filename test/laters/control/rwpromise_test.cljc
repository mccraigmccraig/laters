(ns laters.control.rwpromise-test
  (:require
   [laters.control.rwpromise :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
   [laters.control.either :as either]))

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
                 (error/catch (fn [e] (let [{{x :x :as d} :cause/data} (ex-data e)]
                                       (prn "catch data" d)
                                       (m/return (inc x)))))
                 (r/run)
                 deref)))))

  (testing "run-catch"
    (m/with-context sut/rwpromise-ctx
      (is (= {:monad.writer/output nil
              :monad/val 51}

             (-> (m/return 100)
                 (m/bind (fn [_v]
                           (sut/rwpromise-val
                            sut/rwpromise-ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{{x :x :as d} :cause/data} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)
                 deref)))))

  )
