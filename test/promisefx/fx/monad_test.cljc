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


;; (return a) >>= f ≡ f a
;; (bind (return a) f) ≡ (f a)
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

;; m >>= return ≡ m
;; (bind m return) ≡ m
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

;; (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
;; (bind (bind m f) g) ≡ (bind m (fn [x] (bind (f x) g)))
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

(defn run-left-identity-test
  ([ctx run-compare-fn a mf expected-val]
   (run-left-identity-test nil ctx run-compare-fn a mf expected-val))
  ([opts ctx run-compare-fn a mf expected-val]
   (run-compare-fn
    (left-identity-test-mvs opts ctx a mf)
    expected-val)))

(defn run-right-identity-test
  ([ctx run-compare-fn mv expected-val]
   (run-right-identity-test nil ctx run-compare-fn mv expected-val))
  ([opts ctx run-compare-fn mv expected-val]
   (run-compare-fn
    (right-identity-test-mvs opts ctx mv)
    expected-val)))

(defn run-associativity-test
  ([ctx run-compare-fn m f g expected-val]
   (run-associativity-test nil ctx run-compare-fn m f g expected-val))
  ([opts ctx run-compare-fn m f g expected-val]
   (run-compare-fn
    (associativity-test-mvs opts ctx m f g)
    expected-val)))
