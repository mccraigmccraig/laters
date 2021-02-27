(ns promisefx.fx.monad-test
  (:require
   [promisefx.fx.monad :as m]))

;; https://wiki.haskell.org/Monad_laws
;; compute the pairs of monadic values to compare
;; to verify monad laws

;; return+bind are one pair which satisfy these laws
;; reject+catch seem to be another... hence the ability
;; to override the fns used
;;
;; this seems to make sense because:
;; return+bind process the right branch and short-circuit the left
;; reject+catch process the left branch and short-circuit the right

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
