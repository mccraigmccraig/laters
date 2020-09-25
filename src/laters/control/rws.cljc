(ns laters.control.rws
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]))

;; reader+writer+state
(deftype RWS [lifter]
  m/Monad
  (-bind [m mv f]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (let [{w :monad.writer/output
              st' :monad.state/state
              v :monad/val} ((m/lift-untag lifter m mv)
                             {:monad.reader/env env
                              :monad.state/state st})

             {w' :monad.writer/output
              st'' :monad.state/state
              v' :monad/val} ((m/lift-untag lifter m (f v))
                              {:monad.reader/env env
                               :monad.state/state st'})]

         {:monad.writer/output ((fnil into []) w w')
          :monad.state/state st''
          :monad/val v'}))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val v})))

  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{r :monad.reader/env st
          :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val r})))
  (-local [m f mv]
    (m/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       ((m/lift-untag lifter m mv)
        {:monad.reader/env (f r)
         :monad.state/state st}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output [v]
        :monad.state/state st
        :monad/val nil})))
  (-listen [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val ((m/lift-untag lifter m mv)
                    {:monad.reader/env env})})))
  (-pass [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env
          st :monad.state/state}]
       (let [{w :monad.writer/output
              st' :monad.state/state
              pass-val :monad/val} ((m/lift-untag lifter m mv)
                                    {:monad.reader/env env
                                     :monad.state/state st})
             [val f] (m.w/-as-vec pass-val)]
         {:monad.writer/output (f w)
          :monad.state/state st'
          :monad/val val}))))

  m.st/MonadState
  (-get-state [m]
    (m/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st
        :monad/val st})))
  (-put-state [m st']
    (m/tag
     m
     (fn [{r :monad.reader/env
          st :monad.state/state}]
       {:monad.writer/output nil
        :monad.state/state st'
        :monad/val nil}))))

(defmethod m/-lets (.getName RWS)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'asks (fn [f#] (m.r/-asks ~m f#))
    ~'local (fn [f# mv#] (m.r/-local ~m f# mv#))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))
    ~'get-state (fn [] (m.st/-get-state ~m))
    ~'put-state (fn [st'#] (m.st/-put-state ~m st'#))])

(def rws-lifter
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :monad.reader/env w :monad.writer/output st :monad.state/state}]
                         {:monad.writer/output nil :monad.state/state st :monad/val mv}))})

(def rws-ctx (RWS. rws-lifter))

(defn run-rws
  [wmv rws]
  ((m/untag wmv) rws))


(comment

  (m/run-rws
   (m/mlet m.rws/rws-ctx
     [a (return 5)
      _ (tell :foo)
      {b :bar} (ask)
      c (asks :foo)
      st (get-state)
      _ (put-state (assoc st :baz (+ a b c)))
      d (local
         #(assoc % :bar 30)
         (m/mlet m.rws/rws-ctx
           [{a :foo b :bar} (ask)]
           (return (+ a b))))
      _ (tell d)]
     (return [a b c d]))
   {:monad.reader/env {:foo 20 :bar 10}
    :monad.state/state {:fip 12}})

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
   {:monad.reader/env {:bar 10}
    :monad.state/state {:fip 12}}))
