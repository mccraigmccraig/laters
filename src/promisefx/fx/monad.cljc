(ns promisefx.fx.monad
  (:require
   [promisefx.fx.monad.protocols :as p]
   [promisefx.context.protocols :as ctx.p]
   [promisefx.context :as ctx]))

(defn bind'
  ([mv f]
   (p/-bind (ctx.p/-get-context mv) mv f))
  ([m mv f]
   (p/-bind m mv f)))

(defmacro bind
  ([mv f]
   `(p/-bind ~'this-context## ~mv ~f))
  ([m mv f]
   `(p/-bind ~m ~mv ~f)))

(defn return'
  [m v]
  (p/-return m v))

(defmacro return
  ([m v]
   `(p/-return ~m ~v))
  ([v]
   `(p/-return ~'this-context## ~v)))

(defn guard
  [m v]
  (if (some? v)
    (p/-return m v)
    (p/-mzero m)))

#?(:clj
   (defn mlet*
     "mostly taken from funcool/cats.core"
     [m bindings & body]
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
       (if (some? m)
         `(ctx/with-context ~m ~forms)
         `~forms))))

#?(:clj
   (defmacro mlet
     [& args]
     (let [[arg1 arg2 & rest] args]
       (cond
         (vector? arg1) (apply mlet* 'this-context## arg1 arg2 rest)
         (vector? arg2) (apply mlet* arg1 arg2 rest)
         :else (throw (IllegalArgumentException. "1st or second arg must be binding vector"))))))

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
