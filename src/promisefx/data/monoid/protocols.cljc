(ns ^:no-doc promisefx.data.monoid.protocols)

(defprotocol Semigroup
  "A structure with an associative binary operation."
  (-mappend [s sv sv'] "An associative addition operation."))

(defprotocol Monoid
  "A Semigroup which has an identity element with respect to an associative binary operation."
  (-mempty [s] "The identity element for the given monoid."))
