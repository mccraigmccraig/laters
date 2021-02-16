(ns laters.abstract.tagged.protocols)

(defprotocol ITagSource
  (-get-tag [_]))

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITagged
  (-get-value [_]))

;; for a context presenting a tagged interface,
;; implemented by wrapping a plain context
(defprotocol ITaggedCtx
  (^ITagged -tag [_ v]))
