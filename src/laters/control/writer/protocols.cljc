(ns laters.control.writer.protocols)

(defprotocol MonadWriter
  (-tell [m v])
  (-listen [m mv])
  (-pass [m mv]))

(defprotocol MonadWriterPass
  (-as-vec [_]))
