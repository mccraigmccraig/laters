(ns laters.control.writer
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.abstract.runnable :as r]
   [laters.abstract.tagged :as t]
   [laters.abstract.lifter :as l]
   [laters.control.writer.protocols :as writer.p])
  (:import
   [clojure.lang PersistentVector]))

(defmacro tell
  ([m v]
   `(writer.p/-tell ~m ~v))
  ([v]
   `(writer.p/-tell ~'this-monad## ~v)))

(defmacro listen
  ([m mv]
   `(writer.p/-listen ~m ~mv))
  ([mv]
   `(writer.p/-listen ~'this-monad## ~mv)))

(defmacro pass
  ([m mv]
   `(writer.p/-pass ~m ~mv))
  ([mv]
   `(writer.p/-pass ~'this-monad## ~mv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple Writer context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend PersistentVector
  writer.p/MonadWriterPass
  {:-as-vec (fn [this] this)})

;; an unusual writer - values are fns of an arg, returning
;; the output and value... this makes it easy to re-use the
;; fns outside of the protocol to combine wih reader and
;; state for RW, RWS and PRW and PRWS
(deftype Writer []
  m.p/Monad
  (-type [m]
    [::Writer])
  (-bind [m mv f]
    (r/plain-runnable
     (fn [_]
       (let [{w :monad.writer/output
              val :monad/val} (r/run mv {})
             {w' :monad.writer/output
              val' :monad/val} (r/run (f val) {})]
         {:monad.writer/output (into w w')
          :monad/val val'}))))
  (-return [m v]
    (r/plain-runnable
     (fn [_]
       {:monad.writer/output nil
        :monad/val v})))

  writer.p/MonadWriter
  (-tell [m v]
    (r/plain-runnable
     (fn [_]
       {:monad.writer/output [v]
        :monad/val nil})))
  (-listen [m mv]
    (r/plain-runnable
     (fn [_]
       (let [{w :monad.writer/output
              val :monad/val
              :as lv} (r/run mv {})]
         {:monad.writer/output w
          :monad/val lv}))))
  (-pass [m mv]
    (r/plain-runnable
     (fn [_]
       (let [{w :monad.writer/output
              pass-val :monad/val} (r/run mv {})
             [val f] (writer.p/-as-vec pass-val)]
         {:monad.writer/output (f w)
          :monad/val val})))))

(def writer-ctx (->Writer))

(comment

  ;; the code that's written
  (defwfn fname
    [args]
    [a b]
    (m/return a b))

  ;; the macro produces an fn and a wrapper macro

  ;; the fn
  (defn -fname
    [ctx args]
    (m/mlet ctx
      [a b]
      (m/return a b)))

  ;; the wrapper macro just supplies the monad context arg
  (defmacro fname
    [args]
    `(-fname ~'this-monad## ~@args))
  )


(comment
  (require '[laters.abstract.monad :as m])
  (require '[laters.abstract.runnable :as r])
  (require '[laters.control.writer :as m.writer])

  (r/run
    (m/mlet m.writer/writer-ctx
     [_ (m.writer/tell :foo)
      _ (m.writer/tell :bar)]
     (m/return 100)))

  (r/run
    (m/mlet m.writer/writer-ctx
      [_ (m.writer/tell :foo)
       a (m/return 1)
       b (m/return 2)
       _ (m.writer/tell (+ a b))]
      (m/return [a b])))

  (r/run
    (m/mlet m.writer/writer-ctx
      [v (m.writer/listen (m/mlet m.writer/writer-ctx
                            [_ (m.writer/tell :foo)
                             _ (m.writer/tell :bar)]
                            (m/return 3)))]
      (m/return v)))

  (r/run
    (m/mlet m.writer/writer-ctx
      [v (m.writer/pass (m/mlet m.writer/writer-ctx
                          [_ (m.writer/tell :foo)
                           _ (m.writer/tell :bar)
                           _ (m.writer/tell :foo)]
                          (m/return [3 #(filterv (fn [v] (= v :foo)) %)])))]
      (m/return v)))
)
