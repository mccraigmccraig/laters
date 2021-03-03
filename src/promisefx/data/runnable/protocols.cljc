(ns ^:no-doc promisefx.data.runnable.protocols)

(defprotocol IRunnable
  (-run [mv arg]))

(extend-protocol IRunnable
  nil
  (-run [mv arg]
    mv))
