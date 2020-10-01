(ns laters.demo
  (:require
   [clojure.java.io :as io]
   [laters.abstract.monad :as m]
   [laters.abstract.error :as m.e]
   [laters.abstract.lifter :as lifter]
   [laters.abstract.tagged :as tagged]
   [laters.control.promise :as m.p]
   [laters.control.reader :as m.r]
   [laters.control.writer :as m.w]
   [laters.control.prw :as m.prw]
   [laters.concurrency.promise.promesa :as promesa-impl]
   [promesa.core :as promesa]))

(def lifter (lifter/create-atomic-lifter))

(def promesa-prw-ctx
  (m.prw/make-prw-ctx promesa-impl/factory lifter))

(def promesa-prw-ctx-lifters
  {m.p/promise-ctx (fn [p]
                     (fn [_]
                       (promesa/then
                        p
                        (fn [v]
                          {:monad/val v
                           :monad.writer/output []}))))})

(lifter/register-all lifter promesa-prw-ctx promesa-prw-ctx-lifters)

(defmacro io
  "do all the i/o in a future to simulate real async"
  [& body]
  `(tagged/tag m.p/promise-ctx (promesa/future ~@body)))

(defn spit-file
  [f contents]
  (m/mlet promesa-prw-ctx
    [_ (m.w/tell [:write {:f f}])
     out (io (io/writer f))
     _ (io (.write out contents))
     _ (io (.close out))]

    (m/return :ok)))

(defn slurp-file
  [f]
  (m/mlet promesa-prw-ctx
    [_ (m.w/tell [:read {:f f}])
     contents (io (slurp f))]
    (m/return contents)))

(defn stuff
  []
  (m/mlet promesa-prw-ctx
    [{f1 :f1
      f2 :f2} (m.r/ask)
     _ (spit-file f1 "foofoo")
     txt1 (slurp-file f1)
     txt2 (slurp-file f2)]
    (m/return (str txt1 "-" txt2))))

(defn run
  [f1 f2]
  (m.prw/run-prw
   (m/mlet promesa-prw-ctx
     (m.e/catch
         (fn [e] [:error (.getMessage e)])
         (stuff)))
   {:monad.reader/env {:f1 f1 :f2 f2}}))
