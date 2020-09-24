(ns laters.control.rw
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]))

;; reader+writer
(deftype RW [lifter]
  m/Monad
  (-bind [m mv f]
    (m/tag
     m
     (fn [{r :reader :as arg}]
       (let [{w :writer v :val} ((m/lift-untag lifter m tmv) arg)
             {w' :writer v' :val} ((m/lift-untag lifter m (f v)) arg)]
         {:writer ((fnil into []) w w') :val v'}))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :reader}]
       {:writer nil :val v})))

  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{r :reader}]
       {:writer nil :val r})))
  (-asks [m f]
    (m/tag
     m
     (fn [{r :reader}]
       {:writer nil :val (f r)})))
  (-local [m f mv]
    (m/tag
     m
     (fn [{r :reader}]
       ((m/lift-untag lifter m mv) {:reader (f r)}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :reader w :writer}]
       {:writer [v] :val nil})))
  (-listen [m mv]))

(defmethod m/-lets (.getName RW)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'asks (fn [f#] (m.r/-asks ~m f#))
    ~'local (fn [f# mv#] (m.r/-local ~m f# mv#))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))])

(def rw-lifter
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :reader w :writer}]
                         {:writer nil :val mv}))})

(def rw-ctx (RW. rw-lifter))

(defn run-rw
  [tmv rw]
  ((m/untag tmv) rw))


(comment

  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (return 5)
      _ (tell :foo)
      {b :bar} (ask)
      c (asks :foo)
      d (local
         #(assoc % :foo 10)
         (m/mlet m.rw/rw-ctx
           [{a :foo b :bar} (ask)]
           (return (* a b))))
      ]
     (return [(+ a b) c d]))
   {:reader {:foo 55 :bar 10}})

  ;; auto-lifting
  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (m/mlet m.id/identity-ctx [a (return 10)] (return a))
      _ (tell :foo)
      {b :bar} (ask)]
     (return (+ a b)))
   {:reader {:bar 10}}))
