(ns laters.abstract.monad
  (:require
   [laters.abstract.monad.protocols :as p]))

(defmacro bind
  ([m mv f]
   `(p/-bind ~m ~mv ~f))
  ([mv f]
   `(p/-bind ~'this-monad## ~mv ~f)))

(defmacro return
  ([m v]
   `(p/-return ~m ~v))
  ([v]
   `(p/-return ~'this-monad## ~v)))

(defn guard
  [m v]
  (if (some? v)
    (p/-return m v)
    (p/-mzero m)))

#?(:clj
   (defmacro mlet
     "mostly taken from funcool/cats.core"
     [m bindings-or-first-body & rest]
     (let [[bindings body] (if (vector? bindings-or-first-body)
                             [bindings-or-first-body rest]
                             [nil
                              (concat
                               (list bindings-or-first-body)
                               rest)])]
       (when (and (vector? bindings)
                  (not (even? (count bindings))))
         (throw (IllegalArgumentException. "bindings must have an even number of elements.")))
       (let [forms (->> (reverse (partition 2 bindings))
                        (reduce (fn [acc [l r]]
                                  (case l
                                    :let  `(let ~r ~acc)
                                    :when `(bind ~m
                                                 (guard ~m ~r)
                                                 (fn [~(gensym)] ~acc))
                                    `(bind ~m ~r (fn [~l] ~acc))))
                                `(do ~@body)))]
         `(let [~'this-monad## ~m]
            ~forms)))))

#?(:clj
   (defmacro deflets
     "given a map of syms to monadic contexts, def a series of
      mlet shortcut macros"
     [letsym-ctx-map]
     `(do
        ~@(for [[sym ctx] letsym-ctx-map]
            `(defmacro ~(symbol (name sym))
               ([~'& body#]
                `(mlet ~'~ctx ~@body#)))))))
