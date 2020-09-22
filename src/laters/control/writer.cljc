(ns laters.control.writer
  (:require
   [laters.abstract.monad :as m]))


(defprotocol MonadWriter
  (-tell [m v])
  (-listen [m mv]))

(deftype Writer []
  m/Monad
  (-bind [m wmv f]
    (let [{val :val w :w} (m/untag wmv)
          {val' :val w' :w} (m/untag (f val))]
      (m/tag
       m
       {:val val' :w (into w w')})))
  (-return [m v]
    (m/tag
     m
     {:val v :w nil}))

  MonadWriter
  (-tell [m v]
    (m/tag m {:val nil :w [v]}))
  (-listen [m mv]
    (m/tag m {:val (m/untag mv) :w nil})))

(defmethod m/-lets (.getName Writer)
  [_ m]
  `[~'tell (fn [v#] (-tell ~m v#))
    ~'listen (fn [mv#] (-listen ~m mv#))])

(def writer-ctx (Writer.))

(comment
  (m/mlet m.writer/writer-ctx
    [_ (tell :foo)
     a (return 1)
     b (return 2)
     _ (tell (+ a b))]
    (return [a b]))

  (m/mlet m.writer/writer-ctx
    [v (listen (m/mlet m.writer/writer-ctx
                 [_ (tell :foo)
                  _ (tell :bar)]
                 (return 3)))]
    (return v))
)
