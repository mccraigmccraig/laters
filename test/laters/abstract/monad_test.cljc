(ns laters.abstract.monad-test
  (:require
   [laters.abstract.monad :as m]))

;; https://wiki.haskell.org/Monad_laws
;; compute the pairs of monadic values to compare
;; to verify monad laws

(defn left-identity-test-mvs
  ([ctx a mf]
   (left-identity-test-mvs {} ctx a mf))
  ([{bind :bind
     return :return
     :or {bind m/bind'
          return m/return'}}
    ctx
    a
    mf]

   [(bind ctx (return ctx a) mf)
    (mf a)]))

(defn right-identity-test-mvs
  ([ctx mv] (right-identity-test-mvs {} ctx mv))
  ([{bind :bind
     return :return
     :or {bind m/bind'
          return m/return'}}
    ctx
    mv]

   [(bind ctx mv #(return ctx %))
    mv]))

(defn associativity-test-mvs
  ([ctx m f g] (associativity-test-mvs {} ctx m f g))
  ([{bind :bind
     _return :return
     :or {bind m/bind'
          _return m/return'}}
    ctx
    m
    f
    g]
   [(bind ctx
     (bind ctx m f)
     g)
    (bind
     ctx
     m
     (fn [x]
       (bind
        ctx
        (f x)
        g)))]))
