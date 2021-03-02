(ns ^:no-doc promisefx.fx.reader.protocols)

(defprotocol MonadReader
  (-ask [m])
  (-asks [m f])
  (-local [m f mv]))
