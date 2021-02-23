(ns laters.abstract.monad-test
  (:require
   [laters.abstract.monad :as m]))

;; https://wiki.haskell.org/Monad_laws
;; compute the pairs of monadic values to compare
;; to verify monad laws

(defn left-identity-test-mvs
  [ctx a mf]
  (m/with-context ctx

    [(m/bind (m/return a) mf)
     (mf a)]))

(defn right-identity-test-mvs
  [ctx mv]
  (m/with-context ctx

    [(m/bind mv #(m/return ctx %))
     mv]))

(defn associativity-test-mvs
  [ctx m f g]
  (m/with-context ctx

    [(m/bind
      (m/bind m f)
      g)
     (m/bind
      m
      (fn [x]
        (m/bind
         (f x)
         g)))]))
