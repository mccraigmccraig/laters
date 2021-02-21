(ns laters.abstract.monad-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [laters.abstract.monad :as m]))

;; (= (bind (return a) f)
;;    (f a))
(defn left-identity-test
  [ctx a f]
  (let [x (m/mlet ctx
                  [a' (m/return a)]
                  (f a'))

        y (m/mlet ctx
                  (f a))]

    (t/is (= x y))))

;;(= (bind m return)
;;    m)
(defn right-identity-test
  [ctx]
  (let [m (m/return ctx 100)

        a (m/mlet ctx
                  [x m]
                  (m/return x))

        b (m/mlet ctx
                  m)]
    (t/is (= a b))))

;; (= (bind (bind m f) g)
;;     
;;    )
(defn associativity-test
  [ctx]
  (let [m (m/return ctx 100)
        f #(m/return ctx (* 2 %))
        g #(m/return ctx (- % 50))

        a (m/mlet ctx
                  [y (m/mlet ctx
                             [x m]
                             (f x))]
                  (g y))

        b (m/mlet ctx
                  [x m]
                  (m/mlet ctx
                          [y (f x)]
                          (g y)))

        c (m/mlet ctx
                  [x m
                   y (f x)]
                  (g y))]

    (t/is (= a b c))))
