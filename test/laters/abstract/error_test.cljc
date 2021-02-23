(ns laters.abstract.error-test
  (:require
   [laters.abstract.monad :as m]
   [laters.abstract.error :as error]))

;; catch and reject seem to follow
;; the same https://wiki.haskell.org/Monad_laws
;; as bind and return

(defn left-identity-test-mvs
  [ctx a mf]
  (m/with-context ctx

    [(error/catch (error/reject a) mf)
     (mf a)]))

(defn right-identity-test-mvs
  [ctx mv]
  (m/with-context ctx

    [(error/catch mv #(error/reject ctx %))
     mv]))

(defn associativity-test-mvs
  [ctx m f g]
  (m/with-context ctx

    [(error/catch
      (error/catch m f)
      g)
     (error/catch
      m
      (fn [x]
        (error/catch
         (f x)
         g)))]))
