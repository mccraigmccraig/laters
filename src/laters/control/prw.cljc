(ns laters.control.prw
  (:require
   [laters.abstract.monad :as m]
   [laters.control.identity :as m.id]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.promise :as m.pr]
   [promesa.core :as p]))

;; ({:reader r})->Promise<{:val v :writer w}
(deftype PRW [lifter]
  m/Monad
  (-bind [m wmv f]
    (m/tag
     m
     (fn [{r :reader :as args}]
       (p/chain
        ((m/lift-untag lifter m wmv) args)
        (fn [{w :writer v :val}]
          (p/all [w ((m/lift-untag lifter m (f v)) {:reader r})]))
        (fn [[w {w' :writer v' :val}]]
          (p/resolved
           {:writer ((fnil into []) w w')
            :val v'}))))))
  (-return [m v]
    (m/tag
     m
     (fn [{r :reader}]
       (p/resolved
        {:writer nil :val v}))))
  m/MonadZero
  (-mzero [m]
    (m/tag
     m
     (fn [{r :reader}]
       (p/rejected
        (ex-info
         ":mopr.control.monad/mzero"
         {:writer [::mzero]
          :val nil})))))
  m.r/MonadReader
  (-ask [m]
    (m/tag
     m
     (fn [{r :reader}]
       (p/resolved
        {:writer nil :val r}))))

  m.w/MonadWriter
  (-tell [m v]
    (m/tag
     m
     (fn [{r :reader}]
       (p/resolved
        {:writer [v] :val nil}))))
  (-listen [m mv]))

(defmethod m/-lets (.getName PRW)
  [_ m]
  `[~'ask (fn [] (m.r/-ask ~m))
    ~'tell (fn [v#] (m.w/-tell ~m v#))
    ~'listen (fn [mv#] (m.w/-listen ~m mv#))])

(def prw-lifters
  {m.id/identity-ctx (fn [mv]
                       (fn [{r :reader}]
                         (p/resolved
                          {:writer nil :val mv})))
   m.pr/promise-ctx (fn [mv]
                      (fn [{r :reader}]
                        (p/chain
                         mv
                         (fn [v]
                           {:writer nil :val v}))))})

(def prw-ctx (PRW. prw-lifters))

(defn run-prw
  [wmv rw]
  ((m/untag wmv) rw))


(comment

  @(m/run-prw
    (m/mlet m.prw/prw-ctx
      [a (ask)
       b (return 22)
       c (return (+ a b))
       _ (tell c)
       d (m/mlet m.pr/promise-ctx
           [a (return 100)
            b (return 100)]
           (return (* a a)))]
      (return (+ c d)))
    {:reader 10})

  )
