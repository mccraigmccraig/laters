(ns laters.control.rw
  (:require
   [laters.abstract.monad.protocols :as m.p]
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]))

;; reader+writer
(deftype RW [lifter]
  m.p/Monad
  (-bind [m mv f]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (let [{w :monad.writer/output
              v :monad/val} ((m/lift-untag lifter m mv)
                             {:monad.reader/env env})
             {w' :monad.writer/output
              v' :monad/val} ((m/lift-untag lifter m (f v))
                              {:monad.reader/env env})]
         {:monad.writer/output ((fnil into []) w w')
          :monad/val v'}))))
  (-return [m v]
    (m/tag
     m
     (fn [_]
       {:monad.writer/output nil
        :monad/val v})))

  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       {:monad.writer/output nil
        :monad/val env})))
  (-local [m f mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       ((m/lift-untag lifter m mv) {:monad.reader/env (f env)}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       {:monad.writer/output [v]
        :monad/val nil})))
  (-listen [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (let [{w :monad.writer/output
              v :monad/val
              :as lv} ((m/lift-untag lifter m mv)
                       {:monad.reader/env env})]
         {:monad.writer/output w
          :monad/val lv}))))
  (-pass [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (let [{w :monad.writer/output
              pass-val :monad/val} ((m/lift-untag lifter m mv)
                                    {:monad.reader/env env})
             [val f] (m.w/-as-vec pass-val)]
         {:monad.writer/output (f w)
          :monad/val val})))))

(def rw-lifter
  {m.id/identity-ctx
   (fn [mv]
     (fn [{r :monad.reader/env}]
       {:monad.writer/output nil
        :monad/val mv}))})

(def rw-ctx (RW. rw-lifter))

(defn run-rw
  [tmv rw]
  ((m/untag tmv) rw))

(comment

  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (m/return 5)
      {b :bar} (m.reader/ask)
      c (m.reader/asks :foo)]
     (m/return [a b c]))
   {:monad.reader/env {:foo 55 :bar 10}})

  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (m/return 5)
      _ (m.writer/tell :foo)
      {b :bar} (m.reader/ask)
      c (m.reader/asks :foo)
      d (m.reader/local
         #(assoc % :foo 10)
         (m/mlet m.rw/rw-ctx
           [{a :foo b :bar} (m.reader/ask)]
           (m/return (* a b))))
      ]
     (m/return [(+ a b) c d]))
   {:monad.reader/env {:foo 55 :bar 10}})

  ;; auto-lifting
  (m.rw/run-rw
   (m/mlet m.rw/rw-ctx
     [a (m/mlet m.id/identity-ctx [a (m/return 10)] (m/return a))
      _ (m.writer/tell :foo)
      {b :bar} (m.reader/ask)]
     (m/return (+ a b)))
   {:monad.reader/env {:bar 10}}))
