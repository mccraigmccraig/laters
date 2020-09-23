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
  (-bind [m tmv f]
    (let [mv (m/lift-untag lifter m tmv)]
      (m/tag
       m
       (fn [{r :reader st :state :as rst}]
         (let [{w :writer st' :state v :val} (mv rst)
               {st'' :state
                w' :writer} ((m/lift-untag lifter m (f v)) {:reader r :state st'})]
           {:writer ((fnil into []) w w')
            :state st''})))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer nil :state st :val v})))

  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer nil :state st :val r})))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer [v] :state st :val nil})))
  (-listen [m mv]))

(defmethod m/-lets (.getName RW)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
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
      {b :bar} (ask)]
     (return (+ a b)))
   {:reader {:bar 10}})

  ;; auto-lifting
  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (m/mlet m.id/identity-ctx [a (return 10)] (return a))
      _ (tell :foo)
      {b :bar} (ask)]
     (return (+ a b)))
   {:reader {:bar 10}}))
