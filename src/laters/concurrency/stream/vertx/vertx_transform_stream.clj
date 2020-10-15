(ns laters.concurrency.stream.vertx.vertx-transform-stream
  (:import
   [io.vertx.core Handler]
   [io.vertx.core.streams ReadStream]
   [io.vertx.core.streams.impl PipeImpl]))

(def modes [::flowing ::paused ::ended ::errored])
(def terminal-modes #{::ended ::errored})

(def initial-state
  {::mode ::flowing
   ::demand 0
   ::error nil})

(defmacro when-non-terminal
  [state & body]
  `(let [{mode# ::mode
          :as state#} ~state]
     (if-not (terminal-modes mode#)
       ~@body
       state#)))

(defn pause->state
  [state]
  (when-non-terminal state
    (assoc state ::mode ::paused)))

(defn resume->state
  [state]
  (when-non-terminal state
    (assoc state ::mode ::flowing)))

(defn fetch->state
  [state amount]
  (when-non-terminal state
    (update state ::demand #(+ % amount))))

(defn handled->state
  [state amount]
  (when-non-terminal state
    (update state
            ::demand
            (fn [d]
              (max 0
                   (- d amount))))))

(defn end->state
  [state]
  (when-non-terminal state
    (assoc state ::mode ::ended)))

(defn exception->state
  [state error]
  (when-non-terminal state
    (assoc state
           ::mode ::errored
           ::error error)))

(deftype XFormReadStream [src
                          state-a
                          handler-a
                          endhandler-a
                          exceptionhandler-a]
  ReadStream

  (handler [this handler]
    (reset! handler-a handler))

  (endHandler [this handler]
    (reset! endhandler-a handler)
    (when (and (some? handler)
               (= ::ended (::mode @state-a)))
      (.handle handler nil)))

  (exceptionHandler [this handler]
    (reset! exceptionhandler-a handler)
    (when (and (some? handler)
               (= ::errored (::mode @state-a)))
      (.handle handler (:error @state-a))))

  (pause [this]
    (swap! state-a pause->state)

    ;; pause the source when there is no demand
    (when (and (= ::paused (::mode @state-a))
               (<= (::demand @state-a) 0))
      (.pause src)))

  (fetch [this amount]
    (swap! state-a fetch->state amount)

    (when (and (= ::paused (::mode @state-a))
               (> (::demand @state-a) 0))
      ;; resume the source to fulfil the demand
      (.resume src)))

  (resume [this]
    (swap! state-a resume->state)

    (when (= ::flowing (::mode @state-a))
      (.resume src)))

  (pipe [this]
    (PipeImpl. this))

  (pipeTo [this dst]
    (.pipeTo this dst nil))

  (pipeTo [this dst handler]
    (let [p (PipeImpl. this)]
      (.to p dst handler))))

(defn transform-stream
  "returns a new ReadStream with elements
   transformed by the transducer xform"
  [^ReadStream src xform]
  (let [state-a (atom initial-state)
        endhandler-a (atom nil)
        exceptionhandler-a (atom nil)
        handler-a (atom nil)

        ;; get a reducing fn we can use to do the transformation
        xf (xform conj)]

    (.handler
     src
     (reify Handler
       (handle [_ event]
         (let [r (xf [] event)]
           (when (not-empty r)
             (when-let [handler (deref handler-a)]
               (doseq [v r]
                 (.handle handler v))
               (swap! state-a handled->state (count r))

               (when (and (= ::paused (::mode @state-a))
                          (<= (::demand @state-a) 0))
                 ;; demand is satisfied
                 (.pause src))))))))

    (.endhandler
     src
     (reify Handler
       (handle [_ event]
         (swap! state-a end->state)

         ;; send final values from a stateful transducer
         ;; downstream
         (let [r (xf [])]
           (when (not-empty r)
             (when-let [handler @handler-a]
               (doseq [v r]
                 (.handle handler v)))))

         (some-> endhandler-a
                 deref
                 (.handle event)))))

    (.exceptionhandler
     src
     (reify Handler
       (handle [_ error]
         (swap! state-a exception->state error)

         (some-> exceptionhandler-a
                 deref
                 (.handle error)))))

    (->XFormReadStream
     src
     state-a
     handler-a
     endhandler-a
     exceptionhandler-a)))
