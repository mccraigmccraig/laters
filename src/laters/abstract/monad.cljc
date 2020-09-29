(ns laters.abstract.monad
  (:import
   [clojure.lang Associative]))

;; for contexts to wrap their monadic values in
;; a marker type - and support generic lifts
(defprotocol ITaggedMV
  (-ctx [_])
  (-mv [_]))

(defrecord TaggedMV [ctx mv]
  ITaggedMV
  (-ctx [_] ctx)
  (-mv [_] mv))

(defn tag
  [ctx mv]
  (TaggedMV. ctx mv))

(defn untag
  [bmv]
  (-mv bmv))

(defprotocol Monad
  (-bind [m mv f])
  (-return [m v]))

(defmacro bind
  [mv f]
  `(-bind ~'this-monad## ~mv ~f))

(defmacro return
  ([m v]
   `(-return ~m ~v))
  ([v]
   `(-return ~'this-monad## ~v)))

(defn bind
  [m mv f]
  (-bind m mv f))

(defprotocol ILifter
  (-lift [_ m tmv])
  (-lift-untag [_ m tmv]))

(defn ^:private assoc-lift*
  [lifters m tmv]
  (if (contains? lifters (-ctx tmv))
    ((get lifters (-ctx tmv)) (untag tmv))

    (throw
     (ex-info
      "map lifter: no lifter registered"
      {:from (-ctx tmv)
       :to m
       :tmv tmv}))))

(extend Associative
  ILifter
  {:-lift-untag assoc-lift*
   :-lift (fn [this m tmv]
            (tag m (assoc-lift* this m tmv)))})

(defprotocol IAtomicLifter
  (-register [_ from to lifter])
  (-deregister [_ from to]))

;; a lifter which has an atom of
;; {<to-ctx> {<from-ctx> <lifter>}}
;; permitting bi-directional lifts to
;; be established e.g. P<->PRW or PRW<->PRWS
(defrecord AtomicLifter [lifters-a]
  ILifter
  (-lift-untag [_ m tmv]
    (assoc-lift* (get @lifters-a m {}) m tmv))
  (-lift [_ m tmv]
    (tag m (assoc-lift* (get @lifters-a {}) m tmv)))
  IAtomicLifter
  (-register [_ to-ctx from-ctx lifter]
    (swap!
     lifters-a
     assoc-in
     [to-ctx from-ctx]
     lifter))
  (-deregister [_ to-ctx from-ctx]
    (swap!
     lifters-a
     update-in
     to-ctx
     dissoc
     from-ctx)))

(defn create-atomic-lifter
  []
  (AtomicLifter. (atom {})))

(defn lift-untag
  "lifts a TaggedMV into Monad m, returning
   an untagged MV"
  [lifter m tmv]
  (cond
    (= m (-ctx tmv))
    (untag tmv)

    (some? lifter)
    (-lift-untag lifter m tmv)

    :else
    (throw
     (ex-info
      "no lifts"
      {:from (-ctx tmv)
       :to m
       :tmv tmv}))))

(defn lift
  [lifter m tmv]
  (tag m (lift-untag lifter m tmv)))

(defprotocol MonadZero
  (-mzero [m]))

(defn guard
  [m v]
  (if (some? v)
    (-return m v)
    (-mzero m)))

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

(defn deflets*
  [letsym-ctx-map]
  `(do
     ~@(for [[sym ctx] letsym-ctx-map]
         `(defmacro ~(symbol (name sym))
            ([~'& body#]
             `(mlet ~'~ctx ~@body#))))))

#?(:clj
   (defmacro deflets
     "given a map of syms to monadic contexts, def a series of
      mlet shortcut macros"
     [letsym-ctx-map]
     (deflets* letsym-ctx-map)))
