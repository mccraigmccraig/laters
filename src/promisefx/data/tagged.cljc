(ns promisefx.data.tagged
  (:require
   [promisefx.data.tagged.protocols :as tag.p]
   [promisefx.data.runnable.protocols :as runnable.p]))

(defrecord BoxedTagged [tag val]
  tag.p/Tagged
  (-get-tag [_] tag)

  runnable.p/IRunnable
  (-run [_ arg]
    (runnable.p/-run val arg)))

(defn is-boxed-tagged?
  [v]
  (instance? BoxedTagged v))
