(ns laters.abstract.runnable
  (:require
   [laters.abstract.runnable.protocols :as runnable.p]
   [laters.abstract.tagged.protocols :as tagged.p])
  (:import
   [laters.abstract.runnable.protocols IRunnable]
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]
   [clojure.lang IFn]))

(defn run
  ([m] (run m nil))
  ([m arg]
   (runnable.p/-run m arg)))

(deftype PlainRunnable [f]
  runnable.p/IRunnable
  (-run [_ arg]
    (f arg)))

(defn plain-runnable
  [f]
  (PlainRunnable. f))

(defrecord TaggedRunnable [^ITaggedCtx ctx ^IRunnable f]
  tagged.p/ITaggedMv
  (-tagged-ctx [_] ctx)
  (-inner-mv [_] f)

  runnable.p/IRunnable
  (-run [_ arg]
    (runnable.p/-run f arg)))

(defn tagged-runnable
  [ctx f]
  (->TaggedRunnable ctx f))
