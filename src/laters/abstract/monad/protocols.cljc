(ns laters.abstract.monad.protocols)

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITaggedMV
  (-ctx [_])
  (-mv [_]))

(defprotocol Monad
  (-bind [m mv f])
  (-return [m v]))

(defprotocol ILifter
  (-lift [_ m tmv])
  (-lift-untag [_ m tmv]))

(defprotocol IAtomicLifter
  (-register [_ from to lifter])
  (-deregister [_ from to]))

(defprotocol MonadZero
  (-mzero [m]))
