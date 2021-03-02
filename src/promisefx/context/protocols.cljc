(ns promisefx.context.protocols)

(defprotocol Context
  (-get-tag [_]))

(defprotocol Contextual
  (-get-context [_]
   "return the Context within which a value is being processed"))

(defprotocol ILifter
  (-lift [_ to-type from-type utmv]
    "lift an untagged-mv, returning
     an untagged mv"))

(defprotocol ILifterRegistry
  (-match-lifter [_ to-type from-type]
    "match a lifter for a tagged mv")
  (-register [_ to-type from-type lifter])
  (-deregister [_ to-type from-type]))
