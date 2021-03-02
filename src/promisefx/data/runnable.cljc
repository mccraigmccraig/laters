(ns promisefx.data.runnable
  (:require
   [promisefx.data.runnable.protocols :as runnable.p]
   #?(:cljs [promisefx.data.runnable.protocols :refer [IRunnable]]))
  #?(:clj
     (:import
      [promisefx.data.runnable.protocols IRunnable])))

(defn run
  ([^IRunnable mv] (run mv nil))
  ([^IRunnable mv arg]
   (runnable.p/-run mv arg)))
