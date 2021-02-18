(ns laters.abstract.monad.protocols)

(defprotocol Monad
  (-bind [m mv f])
  (-return [m v])
  (-join [m mv]))

(defprotocol MonadZero
  (-mzero [m]))
