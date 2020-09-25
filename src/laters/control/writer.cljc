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

(defmethod m/-lets (.getName Writer)
  [_ m]
  `[~'tell (fn [v#] (-tell ~m v#))
    ~'listen (fn [mv#] (-listen ~m mv#))
    ~'pass (fn [mv#] (-pass ~m mv#))])

(def writer-ctx (Writer. nil))

(defn run-writer
  [mv]
  ((m/untag mv) nil))

(comment
  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [_ (tell :foo)
      _ (tell :bar)]
     (return 100)))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [_ (tell :foo)
      a (return 1)
      b (return 2)
      _ (tell (+ a b))]
     (return [a b])))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [v (listen (m/mlet m.writer/writer-ctx
                  [_ (tell :foo)
                   _ (tell :bar)]
                  (return 3)))]
     (return v)))

  (m.writer/run-writer
   (m/mlet m.writer/writer-ctx
     [v (pass (m/mlet m.writer/writer-ctx
                [_ (tell :foo)
                 _ (tell :bar)
                 _ (tell :foo)]
                (return [3 #(filterv (fn [v] (= v :foo)) %)])))]
     (return v)))
)
