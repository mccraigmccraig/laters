(ns laters.abstract.monad.protocols)

(defprotocol Monad
  (-bind [m mv f])
  (-return [m v]))

(defprotocol MonadZero
  (-mzero [m]))
