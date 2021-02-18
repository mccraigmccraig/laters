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

(defprotocol Semigroup
  "A structure with an associative binary operation."
  (-mappend [s sv sv'] "An associative addition operation."))

(defprotocol Monoid
  "A Semigroup which has an identity element with respect to an associative binary operation."
  (-mempty [s] "The identity element for the given monoid."))
