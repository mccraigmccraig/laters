(ns laters.control.writer
  (:require
   [laters.abstract.monad :as m])
  (:import
   [clojure.lang PersistentVector]))

(defprotocol MonadWriter
  (-tell [m v])
  (-listen [m mv])
  (-pass [m mv]))

(defprotocol MonadWriterPass
  (-as-vec [_]))

(extend PersistentVector
  MonadWriterPass
  {:-as-vec (fn [this] this)})

;; an unusual writer - values are fns of an arg, returning
;; the output and value... this makes it easy to re-use the
;; fns outside of the protocol to combine wih reader and
;; state for RW, RWS and PRW and PRWS
(deftype Writer [lifter]
  m/Monad
  (-bind [m mv f]
    (m/tag
     m
     (fn [_]
       (let [{w :monad.writer/output
              val :monad/val} ((m/lift-untag lifter m mv) {})
             {w' :monad.writer/output
              val' :monad/val} ((m/lift-untag lifter m (f val)) {})]
         {:monad.writer/output (into w w')
          :monad/val val'}))))
  (-return [m v]
    (m/tag
     m
     (fn [_]
       {:monad.writer/output nil
        :monad/val v})))

  MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [_]
       {:monad.writer/output [v]
        :monad/val nil })))
  (-listen [m mv]
    (m/tag
     m
     (fn [_]
       (let [{w :monad.writer/output
              val :monad/val
              :as lv} ((m/lift-untag lifter m mv) {})]
         {:monad.writer/output w
          :monad/val lv}))))
  (-pass [m mv]
    (m/tag
     m
     (fn [_]
       (let [{w :monad.writer/output
              pass-val :monad/val} ((m/lift-untag lifter m mv) {})
             [val f] (-as-vec pass-val)]
         {:monad.writer/output (f w)
          :monad/val val})))))

(defmacro tell
  [v]
  `(-tell ~'this-monad## ~v))

(defmacro listen
  [mv]
  `(-listen ~'this-monad## ~mv))

(defmacro pass
  [mv]
  `(-pass ~'this-monad## ~mv))

(def writer-ctx (Writer. nil))

(defn run-writer
  [mv]
  ((m/untag mv) nil))

(comment
  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [_ (m.writer/tell :foo)
      _ (m.writer/tell :bar)]
     (m/return 100)))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [_ (m.writer/tell :foo)
      a (m/return 1)
      b (m/return 2)
      _ (m.writer/tell (+ a b))]
     (m/return [a b])))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [v (listen (m/mlet m.writer/writer-ctx
                  [_ (m.writer/tell :foo)
                   _ (m.writer/tell :bar)]
                  (m/return 3)))]
     (m/return v)))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [v (m.writer/pass (m/mlet m.writer/writer-ctx
                         [_ (m.writer/tell :foo)
                          _ (m.writer/tell :bar)
                          _ (m.writer/tell :foo)]
                         (m/return [3 #(filterv (fn [v] (= v :foo)) %)])))]
     (m/return v)))
)
