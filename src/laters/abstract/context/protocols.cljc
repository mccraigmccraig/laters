(ns laters.abstract.context.protocols)

(defprotocol Context
  (-get-type [_]
    "return a type descriptor for the Context - must uniquely id the Context"))

(defprotocol Contextual
  (^Context -get-context [_]
   "return the Context within which a value is being processed"))

(defprotocol Extract
  (-extract [_]
    "extract a value from a context"))
