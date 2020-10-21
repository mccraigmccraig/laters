(ns laters.concurrency.promise.protocols)

(defprotocol IPromiseImpl
  (-type [_]
    "a keyword describing the type of promises")

  (-resolved [_ v]
    "return a resolved promise")
  (-rejected [_ err]
    "return a rejected promise")
  (-deferred [_]
    "return an unfulfilled promise"))

(defprotocol IPromise
  (-resolve!
    [p v]
    "resolve deferred promise p with value v.
     returns true if promise was successfully resolved,
     false if it was already resolved or rejected")
  (-reject!
    [p err]
    "reject deferred promise p with err.
     returns true if the promise was successfully rejected,
     false if it was already resolved or rejected")
  (-then
    [p f]
    [p f impl]
    "returns a new promise of the applicatino of
     f to the value of p. f is a (fn [success-val])")
  (-handle
    [p f]
    [p f impl]
    "returns a new promise of the application of of
     f to either the resolve or rejection value of p.
     f is a (fn [success-val error] ...)")
  (-timeout
    [p t]
    [p t tv]
    [p t tv impl]
    "return a new promise which will be fulfilled with the
     fulfillment of p, unless p is not fulfilled within t ms,
     in which case it will be fulfilled with tv or a TimeoutError")
  #?(:clj
     (-deref
      [p]
      "when supported, blocks to return the promise value")))


;; in order to support executors and any other parameterisation of the
;; impl we'll need to decouple monad identity from monad-ctx instance

;; so... use something like
;; [::PRW :promesa] or [::PRW :rsjava] for the monad-id

;; then a particular context can provide arbitrary params, like executor, to an impl

;; yet another way to do it would be to use the reader to set execution params
