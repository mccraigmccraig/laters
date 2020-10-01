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

(m/deflets
  {prw-let laters.demo/promesa-prw-ctx})

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
  (prw-let
    [_ (m.w/tell [:write {:f f}])
     out (io (io/writer f))
     _ (io (.write out contents))
     _ (io (.close out))]

    (m/return :ok)))

(defn slurp-file
  [f]
  (prw-let
    [_ (m.w/tell [:read {:f f}])
     contents (io (slurp f))]
    (m/return contents)))

(defn stuff
  "get the files, :f1 and :f2, to read and write from the environment"
  []
  (prw-let
    [{f1 :f1
      f2 :f2} (m.r/ask)
     _ (spit-file f1 "foofoo")
     txt1 (slurp-file f1)
     txt2 (slurp-file f2)]
    (m/return [:concatenated (str txt1 "-" txt2)])))

(defn run
  "run without any error recovery"
  [f1 f2]
  (m.prw/run-prw
   (stuff)
   {:monad.reader/env {:f1 f1 :f2 f2}}))

(defn run-recover
  "run and log and recover any errors"
  [f1 f2]
  (m.prw/run-prw
   (prw-let
     (m.e/catch
         (fn [e]
           (m/mlet promesa-prw-ctx
             [_ (m.w/tell [:error (.getMessage e)])]
             (m/return [:recovered])))
         (stuff)))
   {:monad.reader/env {:f1 f1 :f2 f2}}))

(comment

  ;; ;;;;;;;;;;;;; a successful run

  @(laters.demo/run "foo" "foo")

  =>

  {:monad.writer/output
   [[:write {:f "foo"}]
    [:read {:f "foo"}]
    [:read {:f "foo"}]],
   :monad/val [:concatenated "foofoo-foofoo"]}

  ;; ;;;;;;;;;;;;; a failure

  @(laters.demo/run "foo" "bar")

  =>

  ;; BOOM! with ex-data

  {:type :laters.control.prw/error
   :monad/error <the-original-exception>
   :monad.writer/output Atom<[[:write {:f "foo"}]
                              [:read {:f "foo"}]
                              [:read {:f "bar"}]]>}

  ;; ;;;;;;;;;;;;; a failure and recovery

  @(laters.demo/run-recover "foo" "bar")

  =>

  {:monad.writer/output
   [[:write {:f "foo"}]
    [:read {:f "foo"}]
    [:read {:f "bar"}]
    [:error "bar (No such file or directory)"]],
   :monad/val [:recovered]}

  ;; ;;;;;;;

  )
