(ns laters.control.reader
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.tagged :as t]
   [laters.abstract.tagged.protocols :as tag.p]
   [laters.abstract.lifter :as l]
   [laters.control.reader.protocols :as reader.p])
  (:import
   [clojure.lang IFn]
   [laters.abstract.tagged.protocols ITaggedMv ITaggedCtx]))

(deftype ReaderMv [f]
  IFn
  (invoke [_ env]
    (f env)))

(defmacro ask
  ([m]
   `(reader.p/-ask ~m))
  ([]
   `(reader.p/-ask ~'this-monad##)))

(defmacro local
  ([m f mv]
   `(reader.p/-local ~m ~f ~mv))
  ([f mv]
   `(reader.p/-local ~'this-monad## ~f ~mv)))

(defmacro asks
  ([m f]
   `(reader.p/-asks ~m ~f))
  ([f]
   `(reader.p/-asks ~'this-monad## ~f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Reader context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Reader []
  m.p/Monad
  (-type [m]
    [::Reader])
  (-bind [m mv f]
    (r/plain-runnable
     (fn [{env :monad.reader/env}]
       (let [{v :monad/val} (r/run mv {:monad.reader/env env})]
         (r/run (f v) {:monad.reader/env env})))))
  (-return [m v]
    (r/plain-runnable
     (fn [_] {:monad/val v})))
  reader.p/MonadReader
  (-ask [m]
    (r/plain-runnable
     (fn [{env :monad.reader/env}] {:monad/val env})))
  (-asks [m f]
    (r/plain-runnable
     (fn [{env :monad.reader/env}] {:monad/val (f env)})))
  (-local [m f mv]
    (r/plain-runnable
     (fn [{env :monad.reader/env}]
       (r/run mv {:monad.reader/env (f env)})))))

(def reader-ctx (Reader.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TaggedReader context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^ITaggedMv tagged-ask
  [^ITaggedCtx ctx]
  (t/tag ctx (reader.p/-ask (tag.p/-inner-ctx ctx))))

(defn ^ITaggedMv tagged-asks
  [^ITaggedCtx ctx f]
  (t/tag ctx (reader.p/-asks (tag.p/-inner-ctx ctx) f)))

(defn ^ITaggedMv tagged-local
  [^ITaggedCtx ctx f tmv]
  (t/tag
   ctx
   (reader.p/-local (tag.p/-inner-ctx ctx) f (t/untag tmv))))

(deftype TaggedReader []
  tag.p/ITaggedCtx
  (-inner-ctx [this] reader-ctx)
  (-tag [this inner-mv]
    (r/tagged-runnable this inner-mv))

  m.p/Monad
  (-type [m]
    (t/tagged-type m))
  (-bind [m mv f]
    (t/tagged-bind m mv f))
  (-return [m v]
    (t/tagged-return m v))
  reader.p/MonadReader
  (-ask [m]
    (tagged-ask m))
  (-asks [m f]
    (tagged-asks m f))
  (-local [m f mv]
    (tagged-local m f mv)))

(def tagged-reader-ctx (TaggedReader.))

(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.runnable :as r])
  (require '[laters.control.reader :as m.reader])

  (r/run
    (m/mlet m.reader/reader-ctx
     [a (m.reader/ask)
      b (m/return 3)]
     (m/return (+ a b)))
   {:monad.reader/env 10})

  (r/run
   (m/mlet m.reader/reader-ctx
     [a (m.reader/asks :foo)
      b (m/return 3)]
     (m/return (* a b)))
   {:monad.reader/env {:foo 10}})

  (r/run
   (m/mlet m.reader/reader-ctx
     [a (m.reader/asks :foo)
      b (m.reader/local
         #(assoc % :foo 20)
         (m/mlet m.reader/reader-ctx
           [b (m.reader/asks :foo)]
           (m/return b)))]
     (m/return (* a b)))
   {:monad.reader/env {:foo 10}})

  (r/run
    (m/mlet m.reader/tagged-reader-ctx
      [a (m.reader/ask)
       b (m/return 3)]
      (m/return (+ a b)))
    {:monad.reader/env 10})
  )
