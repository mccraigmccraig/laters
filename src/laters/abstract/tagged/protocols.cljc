(ns laters.abstract.tagged.protocols)

(declare ITaggedCtx)

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITaggedMv
  (^ITaggedCtx -tagged-ctx [_])
  (-inner-mv [_]))

;; for a context presenting a tagged interface,
;; implemented by wrapping a plain context
(defprotocol ITaggedCtx
  (^ITaggedMv -tag [_ inner-mv])
  (-inner-ctx [_]))
