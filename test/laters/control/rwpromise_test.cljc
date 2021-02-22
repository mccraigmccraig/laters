(ns laters.control.rwpromise-test
  (:require
   [laters.control.rwpromise :as sut]
   [clojure.test :as t ]
   #?(:clj [clojure.test :as t :refer [deftest testing is]]
      :cljs [cljs.test :as t :refer-macros [deftest testing is]])
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.error :as error]
   [laters.abstract.monad-test :as m.t]))

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
                           (sut/rwpromise-val
                            sut/rwpromise-ctx
                            (fn [_]
                              (throw (ex-info "boo" {:x 50}))))))
                 (error/catch (fn [e] (let [{{x :x :as d} :cause/data} (ex-data e)]
                                       (m/return (inc x)))))
                 (r/run)
                 deref)))))

  ;; TODO - how to preserve writer output in catch etc

  )

(defn run-deref
  [mv arg]
  (deref
   (r/run mv arg)))

(deftest monad-law-test
  (testing "left-identity"
    (let [[a b :as mvs] (m.t/left-identity-test-mvs
                         sut/rwpromise-ctx
                         10
                         (fn [v] (m/return sut/rwpromise-ctx (inc v))))

          [{a-val :monad/val}
           {b-val :monad/val}] (map #(run-deref % {:monad.reader/env :foo}) mvs)]
      (is (= 11 a-val))
      (is (= a-val b-val))))
  (testing "right-identity"
    (let [[a b :as mvs] (m.t/right-identity-test-mvs
                         sut/rwpromise-ctx
                         (sut/plain-rwpromise-val sut/rwpromise-ctx :foo))

          [{a-val :monad/val}
           {b-val :monad/val}] (map #(run-deref % {:monad.reader/env :foo}) mvs)]
      (is (= :foo a-val))
      (is (= a-val b-val))))
  (testing "associativity"
    (let [[a b :as mvs] (m.t/associativity-test-mvs
                         sut/rwpromise-ctx
                         (sut/plain-rwpromise-val sut/rwpromise-ctx "foo")
                         #(m/return sut/rwpromise-ctx (str % "bar"))
                         #(m/return sut/rwpromise-ctx (str % "baz")))
          [{a-val :monad/val}
           {b-val :monad/val}] (map #(run-deref % {:monad.reader/env :foo}) mvs)]
      (is (= "foobarbaz" a-val))
      (is (= a-val b-val)))))
