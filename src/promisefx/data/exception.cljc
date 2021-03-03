(ns promisefx.data.exception
  #?(:clj
     (:import
      [java.util.concurrent
       ExecutionException
       CompletionException])))

(def ^:private exception-wrapper-classes
  #?(:clj #{ExecutionException CompletionException}
     :cljs #{}))

(defn platform-exception-wrapper?
  [e]
  #?(:clj (contains? exception-wrapper-classes (some-> e .getClass))
     :cljs false))

(defn unwrap-exception
  "remove j.u.c exception wrappers"
  [e]
  (if (platform-exception-wrapper? e)
    (ex-cause e)
    e))
