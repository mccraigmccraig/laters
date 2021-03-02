(ns ^:no-doc promisefx.fx.monad.protocols)

(defprotocol Monad
  (-bind [m mv f])
  (-return [m v])
  (-join [m mv]))

(defprotocol MonadZero
  (-mzero [m]))
