(ns laters.abstract.error.protocols)

(defprotocol MonadError
  (-reject [m v])
  (-catch [m mv f])
  (-finally [m mv f]))

(extend-protocol MonadError
  Object
  (-reject [m v]
    (throw (ex-info "MonadError not implemented" {:m m :v v})))
  (-catch [m mv f]
    (throw (ex-info "MonadError not implemented" {:m m :mv mv :f f})))
  (-finally [m mv f]
    (throw (ex-info "MonadError not implemented" {:m m :mv mv :f f})))

  nil
  (-reject [m v]
    (throw (ex-info "MonadError not implemented" {:m m :v v})))
  (-catch [m mv f]
    (throw (ex-info "MonadError not implemented" {:m m :mv mv :f f})))
  (-finally [m mv f]
    (throw (ex-info "MonadError not implemented" {:m m :mv mv :f f}))))
