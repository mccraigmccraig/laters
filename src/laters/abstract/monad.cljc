(ns laters.abstract.monad)

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
  (-return [m v])
  (-lift [m wmv]))

(defmulti -lets
  (fn [ctx-classname ctx]
    ctx-classname))

(defmethod -lets :default
  [ctx-classname ctx]
  nil)

(defn bind
  [m mv f]
  (-bind m mv f))

(defn lift
  "a lifter is a fn which takes an TaggedMV mv and
   lifts it into Monad m"
  [m lifters wmv]
  (cond
    (= m (-ctx wmv))
    wmv

    (contains? lifters (-ctx wmv))
    (tag
     m
     ((get lifters (-ctx wmv)) (untag wmv)))

    :else
    (throw
     (ex-info
      "no lifter registered"
      {:from (-ctx wmv)
       :to m
       :wmv wmv}))))

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
                              `(do ~@body)))
           lets (into
                 `[~'ctx ~m
                   ~'return (fn ~'return [v#]
                              (-return ~m v#))]
                 ;; will this work on cljs ? it requires the macro
                 ;; to eval the clj version of the protocol
                 (-lets (-> (eval m) type .getName) m))]
       `(let ~lets
          ~forms))))
