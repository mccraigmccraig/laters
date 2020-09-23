(ns laters.control.prws
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.state :as m.st]
   [laters.control.promise :as m.pr]
   [promesa.core :as p]))


;; ({:reader r :state st})->Promise<{:val v :writer w :state st}
(deftype PRWS [lifter]
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{r :reader st :state :as r-st}]
       (p/chain
        ((m/lift-untag lifter m wmv) r-st)
        (fn [{w :writer st' :state v :val :as b1}]
          (p/all [w ((m/lift-untag lifter m (f v)) {:reader r :state st'})]))
        (fn [[w {st'' :state w' :writer v' :val :as b2}]]
          (p/resolved
           {:writer ((fnil into []) w w')
            :state st''
            :val v'}))))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/resolved
        {:writer nil :state st :val v}))))
  m/MonadZero
  (-mzero [m]
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/rejected
        (ex-info
         ":mopr.control.monad/mzero"
         {:writer [::mzero]
          :state st
          :val nil})))))

  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/resolved
        {:writer nil :state st :val r}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/resolved
        {:writer [v] :state st :val nil}))))
  (-listen [m mv])

  m.st/MonadState
  (-get-state [m]
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/resolved
        {:writer nil :state st :val st}))))
  (-put-state [m st']
    (m/tag
     m
     (fn [{r :reader st :state}]
       (p/resolved
        {:writer nil :state st' :val nil})))))

(defmethod m/-lets (.getName PRWS)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))
    ~'get-state (fn [] (m.st/-get-state ~m))
    ~'put-state (fn [st'#] (m.st/-put-state ~m st'#))])

(def prws-lifter
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :reader w :writer st :state}]
                         (p/resolved
                          {:writer nil :state st :val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :reader w :writer st :state}]
                        (p/chain
                         mv
                         (fn [v]
                           {:writer nil :state st :val v}))))})

(def prws-ctx (PRWS. prws-lifter))

(defn run-prws
  [wmv rws]
  ((m/untag wmv) rws))

(comment

  @(m/run-prws
    (m/mlet m.prws/prws-ctx
      [a (ask)
       b (get-state)
       _ (put-state a)
       c (return (+ a b))
       _ (tell c)
       d (m/mlet m.pr/promise-ctx
           [a (return 100)
            b (return 100)]
           (return (* a a)))]
      (return d))
    {:reader 10 :state 20})
  )
