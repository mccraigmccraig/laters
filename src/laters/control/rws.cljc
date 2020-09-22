(ns laters.control.rws
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]))

;; reader+writer+state
(deftype RWS [lifters]
  m/Monad
  (-bind [m wmv f]
    (let [wmv (m/-lift m wmv)]
      (m/tag
       m
       (fn [{r :reader st :state :as rst}]
         (let [{w :writer st' :state v :val} ((m/untag wmv) rst)
               {st'' :state
                w' :writer} ((m/untag (m/-lift m (f v))) {:reader r :state st'})]
           {:writer ((fnil into []) w w')
            :state st''})))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer nil :state st :val v})))
  (-lift [m wmv]
    (m/lift m lifters wmv))

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
  (-listen [m mv])

  m.st/MonadState
  (-get-state [m]
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer nil :state st :val st})))
  (-put-state [m st']
    (m/tag
     m
     (fn [{r :reader w :writer st :state}]
       {:writer nil :state st' :val nil}))))

(defmethod m/-lets (.getName RWS)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))
    ~'get-state (fn [] (m.st/-get-state ~m))
    ~'put-state (fn [st'#] (m.st/-put-state ~m st'#))])

(def rws-lifters
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :reader w :writer st :state}]
                         {:writer nil :state st :val mv}))})

(def rws-ctx (RWS. rws-lifters))

(defn run-rws
  [wmv rws]
  ((m/untag wmv) rws))


(comment

  (m/run-rws
   (m/mlet m.rws/rws-ctx
     [a (return 5)
      _ (tell :foo)
      {b :bar} (ask)
      st (get-state)
      _ (put-state (assoc st :baz (+ a b)))]
     (return (+ a b)))
   {:reader {:bar 10}
    :state {:fip 12}})

  ;; auto-lifting
  (m/run-rws
   (m/mlet m.rws/rws-ctx
     [a (m/mlet m.id/identity-ctx [a (return 10)] (return a))
      _ (tell :foo)
      {b :bar} (ask)
      st (get-state)
      _ (tell st)
      _ (put-state (assoc st :baz (+ a b)))]
     (return (+ a b)))
   {:reader {:bar 10}
    :state {:fip 12}}))
