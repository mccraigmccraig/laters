(ns laters.abstract.runnable
  (:require
   [laters.abstract.runnable.protocols :as runnable.p]
   [laters.abstract.tagged.protocols :as tagged.p])
  (:import
   [laters.abstract.runnable.protocols IRunnable]
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]
   [clojure.lang IFn]))

(defn run
  [m & args]
  (runnable.p/-run m args))

(deftype PlainRunnable [f]
  runnable.p/IRunnable
  (-run [_ args]
    (apply f args)))

(defn plain-runnable
  [f]
  (PlainRunnable. f))

(defrecord TaggedRunnable [^ITaggedCtx ctx ^IRunnable f]
  tagged.p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-inner-mv [_] f)

  runnable.p/IRunnable
  (-run [_ args]
    (runnable.p/-run f args)))

(defn tagged-runnable
  [ctx f]
  (->TaggedRunnable ctx f))
