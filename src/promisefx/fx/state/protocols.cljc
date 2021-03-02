(ns ^:no-doc promisefx.fx.state.protocols)

(defprotocol MonadState
  (-get-state [m])
  (-put-state [m st']))
