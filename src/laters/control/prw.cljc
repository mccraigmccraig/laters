(ns laters.control.prw
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.promise :as m.pr]
   [promesa.core :as p]))

;; ({:monad.reader/env r})->Promise<{:monad/val v :monad.writer/output w}
(deftype PRW [lifter]
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/chain
        ((m/lift-untag lifter m wmv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             v :monad/val}]
          (p/all [w ((m/lift-untag lifter m (f v)) {:monad.reader/env env})]))
        (fn [[w
             {w' :monad.writer/output
              v' :monad/val}]]
          (p/resolved
           {:monad.writer/output ((fnil into []) w w')
            :monad/val v'}))))))
  (-return [m v]
    (m/tag
     m
     (fn [_]
       (p/resolved
        {:monad.writer/output nil
         :monad/val v}))))
  m/MonadZero
  (-mzero [m]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/rejected
        (ex-info
         ":mopr.control.monad/mzero"
         {:monad.writer/output [::mzero]
          :monad/val nil})))))
  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        {:monad.writer/output nil
         :monad/val env}))))
  (-local [m f mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       ((m/lift-untag lifter m mv)
        {:monad.reader/env (f env)}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/resolved
        {:monad.writer/output [v] :monad/val nil}))))
  (-listen [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             v :monad/val
             :as lv}]
          {:monad.writer/output w
           :monad/val lv})))))
  (-pass [m mv]
    (m/tag
     m
     (fn [{env :monad.reader/env}]
       (p/chain
        ((m/lift-untag lifter m mv) {:monad.reader/env env})
        (fn [{w :monad.writer/output
             pass-val :monad/val}]
          (let [[val f] (m.w/-as-vec pass-val)]
            {:monad.writer/output (f w)
             :monad/val val})))))))

(def prw-lifters
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :monad.reader/env}]
                         (p/resolved
                          {:monad.writer/output nil :monad/val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :monad.reader/env}]
                        (p/chain
                         mv
                         (fn [v]
                           {:monad.writer/output nil :monad/val v}))))})

(def prw-ctx (PRW. prw-lifters))

(defn run-prw
  [wmv rw]
  ((m/untag wmv) rw))


(comment

  @(m/run-prw
    (m/mlet m.prw/prw-ctx
      [{a :foo} (m.reader/ask)
       b (m.reader/asks :bar)
       c (m/return (+ a b))
       _ (m.writer/tell c)
       d (m.reader/local
          #(assoc % :foo 20)
          (m/mlet m.prw/prw-ctx
            [{a :foo b :bar} (m.reader/ask)]
            (m/return (+ a b))))
       {listen-out :monad.writer/output
        e :monad/val} (m.writer/listen
                       (m/mlet m.prw/prw-ctx
                         [_ (m.writer/tell :foofoo)
                          _ (m.writer/tell :barbar)]
                         (m/return :blah)))
       _ (m.writer/tell listen-out)
       f (m.writer/pass
          (m/mlet m.prw/prw-ctx
            [_ (m.writer/tell :one)
             _ (m.writer/tell :two)
             _ (m.writer/tell :one)]
            (m/return [:passed (fn [out] (filter #(= :one %) out))])))
       g (m/mlet m.pr/promise-ctx
           [a (m/return 100)
            b (m/return 100)]
           (m/return (* a a)))]
      (m/return [a b c d e f g]))
    {:monad.reader/env {:foo 10 :bar 20}})




  )
