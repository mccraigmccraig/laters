(ns laters.abstract.monad.protocols)

(defprotocol Monad
  (-type [m])
  (-bind [m mv f])
  (-return [m v])
  (-join [m mv]))

(defprotocol MonadZero
  (-mzero [m]))
