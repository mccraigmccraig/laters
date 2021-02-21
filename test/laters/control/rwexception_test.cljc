(ns laters.control.rwexception-test
  (:require
   [laters.control.rwexception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
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
