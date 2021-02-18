(ns laters.abstract.runnable
  (:require
   [laters.abstract.runnable.protocols :as runnable.p]))

(defn run
  ([m] (run m nil))
  ([m arg]
   (runnable.p/-run m arg)))
