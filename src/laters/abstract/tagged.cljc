(ns laters.abstract.tagged
  (:require
   [laters.abstract.tagged.protocols :as p]))

(defrecord Tagged [ctx mv]
  p/ITagged
  (-ctx [_] ctx)
  (-mv [_] mv))

(defn tag
  [ctx mv]
  (Tagged. ctx mv))

(defn untag
  [bmv]
  (p/-mv bmv))
