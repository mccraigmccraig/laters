(ns laters.control.state.protocols)

(defprotocol MonadState
  (-get-state [m])
  (-put-state [m st']))
