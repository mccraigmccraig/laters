(ns laters.control.reader.protocols)

(defprotocol MonadReader
  (-ask [m])
  (-asks [m f])
  (-local [m f mv]))
