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
  [v]
  `(-return ~'this-monad## ~v))

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
     [m bindings & body]
     (when-not (and (vector? bindings)
                    (not-empty bindings)
                    (even? (count bindings)))
       (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
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
          ~forms))))
