(ns laters.concurrency.promise.protocols)

(defprotocol IPromiseImpl
  (-type [_]
    "a keyword describing the type of promises")

  (-resolved [_ v]
    "make a resolved promise")
  (-rejected [_ err]
    "make a rejected promise")
  (-deferred [_]
    "make an unfulfilled promise"))

(defprotocol IPromise
  (-resolve!
    [p v])
  (-reject!
    [p err])
  (-then
    [p f]
    [p f impl]
    "f is a (fn [success-val])")
  (-handle
    [p f]
    [p f impl]
    "f is a (fn [success-val error] ...)")
  #?(:clj (-deref [p])))


;; in order to support executors and any other parameterisation of the
;; impl we'll need to decouple monad identity from monad-ctx instance

;; so... use something like
;; [::PRW :promesa] or [::PRW :rsjava] for the monad-id

;; then a particular context can provide arbitrary params, like executor, to an impl

;; yet another way to do it would be to use the reader to set execution params
