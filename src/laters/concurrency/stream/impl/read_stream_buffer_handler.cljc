(ns laters.concurrency.stream.impl.read-stream-buffer-handler
  (:require
   [laters.concurrency.promise :as promise]
   [laters.concurrency.promise.promesa :as promesa]
   [laters.concurrency.stream.protocols :as stream.p])
  (:import
   [java.util ArrayDeque]
   [java.io Writer]))

(def handler-states [::unsubscribed ::subscribed ::completed ::errored])

(defn initial-state
  []
  {::handler-state ::unsubscribed
   ::stream-buffer nil
   ::handle-q (ArrayDeque.)
   ::take-q (ArrayDeque.)
   ::error nil})

(defn fulfil-one-take
  "fulfil a single take if possible"
  [take-q v]
  (if (> (count take-q) 0)
    (loop [{take-r ::take-r} (.poll take-q)]
      (if (true? (promise/resolve! take-r v))
        ::take-fulfilled
        (if (> (count take-q) 0)
          (recur (.poll take-q))
          ::no-take)))
    ::no-take))

(defn fulfil
  "fulfil as many take-q items from the handle-q as possible"
  [handle-q take-q]
  (if (> (count handle-q) 0)
    (loop [{handle-r ::handle-r
            value ::value} (.peek handle-q)]
      (if (= ::take-fulfilled (fulfil-one-take take-q value))
        (do
          (.poll handle-q)
          (if (> (count handle-q) 0)
            (recur (.peek handle-q))
            ::done))
        ::done))
    ::done))

(defn default-remaining-takers
  [take-q]
  (if (> (count take-q) 0)
    (loop [{take-r ::take-r
            default-val ::default-value} (.poll take-q)]
      (promise/resolve! take-r default-val)
      (if (> (count take-q) 0)
        (recur (.poll take-q))
        true))
    true))

(defn error-remaining-takers
  [take-q error]
  (if (> (count take-q) 0)
    (loop [{take-r ::take-r} (.poll take-q)]
      (promise/reject! take-r error)
      (if (> (count take-q) 0)
        (recur (.poll take-q))
        true))
    true))

(deftype ReadStreamBufferHandler [promise-impl
                                  take-q-size
                                  state-a]
  stream.p/IStreamBufferHandler
  (-on-subscribe [this sb]
    (locking this
      (let [{chs ::handler-state
             csb ::stream-buffer
             :as state} @state-a]
        (cond
          (and (some? csb) (some? sb))
          (throw (ex-info "handler already subscribed" state))

          (and (nil? csb) (some? sb) (= ::unsubscribed chs))
          (swap! state-a assoc
                 ::stream-buffer sb
                 ::handler-state ::subscribed)

          (and (nil? csb) (some? sb))
          (throw (ex-info "handler not ::unsubscribed" state))

          (and (some? csb) (nil? sb) (= ::subscribed chs))
          (swap! state-a assoc
                 ::stream-buffer nil
                 ::handler-state ::unsubscribed)

          (and (some? csb) (nil? sb))
          (swap! state-a assoc
                 ::stream-buffer nil)

          :else
          (throw (ex-info "unknown case" state))))))

  (-on-event [this v]
    (locking this
      (let [{handle-q ::handle-q
             take-q ::take-q} @state-a
            handle-r (promise/deferred promise-impl)]

        (if (= ::take-fulfilled (fulfil-one-take take-q v))
          (promise/resolve! handle-r true)

          (.add handle-q {::handle-r handle-r
                          ::value v}))

        handle-r)))

  (-on-complete [this]
    (locking [this]
      (let [{handle-q ::handle-q
             take-q ::take-q} @state-a]
        (swap! state-a assoc ::handler-state ::completed)
        (fulfil handle-q take-q)
        (default-remaining-takers take-q))))

  (-on-error [this err]
    (locking [this]
      (let [{handle-q ::handle-q
             take-q ::take-q} @state-a]
        (swap! state-a assoc
               ::handler-state ::errored
               ::error err)
        (fulfil handle-q take-q)
        (error-remaining-takers take-q err))))

  stream.p/IReadStream
  (-take! [this]
    (stream.p/-take! this nil nil nil))
  (-take! [this timeout]
    (stream.p/-take! this nil timeout nil))
  (-take! [this timeout timeout-val]
    (stream.p/-take! this nil timeout timeout-val))
  (-take! [this default-val timeout timeout-val]
    (locking this
      (let [{handler-state ::handler-state
             stream-buffer ::stream-buffer
             handle-q ::handle-q
             take-q ::take-q
             error ::error} @state-a]

        (cond
          (= ::completed handler-state)
          (promise/resolved promise-impl default-val)

          (= ::errored handler-state)
          (promise/rejected promise-impl error)

          :else
          (let [take-r (promise/deferred promise-impl)
                take-r (if timeout
                         (promise/timeout take-r timeout timeout-val promise-impl)
                         take-r)]
            ;; add to take-q before requesting, so that
            ;; any request gets fulfilled immediately
            (.add take-q {::take-r take-r
                          ::default-value default-val})

            ;; if we have a stream-buffer, request another message
            (when (some? stream-buffer)
              (stream.p/-request! stream-buffer 1))

            (fulfil handle-q take-q)

            take-r)))))

  (-buffer [this buffer-size])
  (-buffer [this buffer-size overflow])

  (-transform [this xform])
  (-transform [this xform impl])

  (-reduce [this f])
  (-reduce [this f initval])
  (-reduce [this f initval impl]))

(defmethod print-method ReadStreamBufferHandler [rsbh ^Writer w]
  (let [{stream-buffer ::stream-buffer
         handle-q ::handle-q
         take-q ::take-q
         error ::error
         :as state} (-> rsbh .-state-a deref)
        print-state (-> state
                        (select-keys [::handler-state])
                        (assoc ::handle-q (count handle-q)
                               ::take-q (count take-q)
                               ::stream-buffer (some? stream-buffer)
                               ::error (some? error)))]
    (.write w "<< read-stream-buffer-handler: ")
    (.write w (prn-str print-state))
    (.write w " >>")))

(defn read-stream-buffer-handler
  ([] (read-stream-buffer-handler {}))
  ([{promise-impl :stream/promise-impl
     take-q-size :stream/take-q-size
     :or {promise-impl promesa/default-impl
          take-q-size 16384}}]
   (let [st (initial-state)]
     (->ReadStreamBufferHandler
      promise-impl
      take-q-size
      (atom st)))))
