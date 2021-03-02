(ns ^:no-doc promisefx.data.runnable.protocols)

(defprotocol IRunnable
  (-run [m arg]))
