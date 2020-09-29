(ns laters.abstract.tagged.protocols)

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITagged
  (-ctx [_])
  (-mv [_]))
