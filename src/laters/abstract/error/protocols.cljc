(ns laters.abstract.error.protocols)

(defprotocol MonadError
  (-reject [m v])
  (-catch [m handler mv]))