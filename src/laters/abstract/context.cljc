(ns laters.abstract.context
  (:require
   [laters.abstract.context.protocols :as p])
  (:import
   [laters.abstract.context.protocols Context Contextual]))

(defn get-context
  [^Contextual v]
  (p/-get-context v))

(defn get-context-type
  [^Contextual v]
  (p/-get-type (p/-get-context v)))

(defn get-type
  [^Context ctx]
  (p/-get-type ctx))
