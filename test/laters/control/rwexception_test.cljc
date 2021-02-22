(ns laters.control.rwexception-test
  (:require
   [laters.control.rwexception :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
   [laters.abstract.monad-test :as m.t]
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

(deftest monad-law-test
  (testing "left-identity"
    (let [[a b :as mvs] (m.t/left-identity-test-mvs
                         sut/rwexception-ctx
                         10
                         (fn [v] (m/return sut/rwexception-ctx (inc v))))

          [{a-val :monad/val}
           {b-val :monad/val}] (map #(r/run % {:monad.reader/env :foo}) mvs)]
      (is (= 11 a-val))
      (is (= a-val b-val))))
  (testing "right-identity"
    (let [[a b :as mvs] (m.t/right-identity-test-mvs
                         sut/rwexception-ctx
                         (sut/plain-rwexception-val sut/rwexception-ctx :foo))

          [{a-val :monad/val}
           {b-val :monad/val}] (map #(r/run % {:monad.reader/env :foo}) mvs)]
      (is (= :foo a-val))
      (is (= a-val b-val))))
  (testing "associativity"
    (let [[a b :as mvs] (m.t/associativity-test-mvs
                         sut/rwexception-ctx
                         (sut/plain-rwexception-val sut/rwexception-ctx "foo")
                         #(m/return sut/rwexception-ctx (str % "bar"))
                         #(m/return sut/rwexception-ctx (str % "baz")))
          [{a-val :monad/val}
           {b-val :monad/val}] (map #(r/run % {:monad.reader/env :foo}) mvs)]
      (is (= "foobarbaz" a-val))
      (is (= a-val b-val)))))


;; TODO need further tests verifying monad-laws for composition over error values
