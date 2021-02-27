(ns promisefx.fx.writer.protocols)

(defprotocol MonadWriter
  (-tell [m v])
  (-listen [m mv])
  (-pass [m mv]))
