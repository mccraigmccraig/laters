(ns laters.abstract.monad.protocols)

(defprotocol Monad
  (-type [m])
  (-bind [m mv f])
  (-return [m v]))

(defprotocol MonadZero
  (-mzero [m]))
