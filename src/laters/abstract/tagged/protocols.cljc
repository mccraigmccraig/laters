(ns laters.abstract.tagged.protocols)

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITaggedMv
  (-tagged-ctx [_])
  (-inner-mv [_]))

;; for a context presenting a tagged interface,
;; implemented by wrapping a plain context
(defprotocol ITaggedCtx
  (-inner-ctx [_])
  (-tag [_ inner-mv]))
