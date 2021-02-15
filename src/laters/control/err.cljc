(ns laters.control.err
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.error.protocols :as err.p]
   [laters.abstract.error :as err]
   [laters.abstract.tagged :as tag]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as lift])
  (:import
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Err context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Err []
  m.p/Monad
  (-type [m]
    [::Identity])
  (-bind [m mv f]
    (if (err.p/error? mv)
      mv
      (try
        (f mv)
        (catch Exception e
          (err/error-marker e)))))
  (-join [m mmv]
    mmv)
  (-return [m v]
    v)

  err.p/MonadError
  (-reject [m v]
    (err/error-marker v))
  (-catch [m mv f]
    (try
      (f mv)
      (catch Exception e
        (err/error-marker e))))
  (-finally [m mv f]
    (try
      mv
      (finally (f)))))


(def err-ctx (->Err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TaggedErr context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype TaggedErr [lifter]
  tag.p/ITaggedCtx
  (-inner-ctx [this] err-ctx)
  (-tag [this inner-mv]
    (t/tagged-plain this inner-mv))

  m.p/Monad
  (-type [m]
    (t/tagged-type m))
  (-bind [m tmv tmf]
    (t/tagged-bind m tmv tmf))
  (-return [m v]
    (t/tagged-return m v))

  err.p/MonadError
  (-reject [m v]
    (err/tagged-reject m v))
  (-catch [m mv f]
    (err/tagged-catch m mv f lifter))
  (-finally [m mv f]
    (err/tagged-finally m mv f)))

(def tagged-err-ctx (->TaggedErr nil))
