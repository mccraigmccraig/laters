(ns promisefx.context
  (:require
   [promisefx.context.protocols :as p]
   #?(:cljs [promisefx.context.protocols :refer [Context Contextual]]))
  #?(:clj
     (:import
      [promisefx.context.protocols Context Contextual])))

(defn get-context
  [^Contextual v]
  (p/-get-context v))

(defn get-context-tag
  [^Contextual v]
  (p/-get-tag (p/-get-context v)))

(defn get-tag
  [^Context ctx]
  (p/-get-tag ctx))

#?(:clj
   (defmacro with-context
     [ctx & body]
     `(let [~'this-context## ~ctx]
        ~@body)))
