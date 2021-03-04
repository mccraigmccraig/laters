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
;; a is any plain value, f is any monadic fn
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
;; m is any monadic value
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
;; m is any monadic value, f and g are any monadic fns
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

(defn run-left-identity-tests
  ([ctx run-compare-fn a-mf-expected-vals]
   (run-left-identity-tests {} ctx run-compare-fn a-mf-expected-vals))
  ([opts ctx run-compare-fn a-mf-expected-vals]
   (doseq [[a mf expected-val] a-mf-expected-vals]
     (run-left-identity-test opts ctx run-compare-fn a mf expected-val))))

(defn run-right-identity-test
  ([ctx run-compare-fn mv expected-val]
   (run-right-identity-test nil ctx run-compare-fn mv expected-val))
  ([opts ctx run-compare-fn mv expected-val]
   (run-compare-fn
    (right-identity-test-mvs opts ctx mv)
    expected-val)))

(defn run-right-identity-tests
  ([ctx run-compare-fn mv-expected-vals]
   (run-right-identity-tests {} ctx run-compare-fn mv-expected-vals))
  ([opts ctx run-compare-fn mv-expected-vals]
   (doseq [[mv expected-val] mv-expected-vals]
     (run-right-identity-test opts ctx run-compare-fn mv expected-val))))

(defn run-associativity-test
  ([ctx run-compare-fn m f g expected-val]
   (run-associativity-test nil ctx run-compare-fn m f g expected-val))
  ([opts ctx run-compare-fn m f g expected-val]
   (run-compare-fn
    (associativity-test-mvs opts ctx m f g)
    expected-val)))

(defn run-associativity-tests
  ([ctx run-compare-fn m-f-g-expected-vals]
   (run-associativity-tests {} ctx run-compare-fn m-f-g-expected-vals))
  ([opts ctx run-compare-fn m-f-g-expected-vals]
   (doseq [[m f g expected-val] m-f-g-expected-vals]
     (run-associativity-test opts ctx run-compare-fn m f g expected-val))))

(defn run-monad-law-tests
  ([ctx run-compare-fn law-testdata]
   (run-monad-law-tests {} ctx run-compare-fn law-testdata))
  ([opts
    ctx
    run-compare-fn
    {left-identity-data :left-identity
     right-identity-data :right-identity
     associativity-data :associativity
     :as law-testdata}]

   (run-left-identity-tests opts ctx run-compare-fn left-identity-data)
   (run-right-identity-tests opts ctx run-compare-fn right-identity-data)
   (run-associativity-tests opts ctx run-compare-fn associativity-data)))
