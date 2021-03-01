# promisefx

an exploration of some other ideas for effectful programming with
clojure/script

effectful programming offers some useful ways of separating the what
of some code from the how

promisefx came into being because some useful things from synchronous environments
(e.g. stacktraces, thread-locals) are not available asynchronous environments,
and i wanted some constructs which could solve the same problems without being
dependent on the execution environment

it brings the following effects:

* error - uniform error handling across different contexts
* reader - an input-only effect
* writer - an output-only effect
* state - a modifiable state effect. i'm not even sure this should be here, but it's approximately free given the implementation, so why not

### error effect

the error effect provides uniform error behaviour in different contexts without impacting other effects. if an error is thrown, then the current computation will short-circuit until it either ends or the error is caught. if an exception is thrown in a context with an error effect then the exception is wrapped in a Failure and will behave as if an error were thrown

the error effect does not impact any other effects. e.g. if an error is thrown in a computation which also has a writer effect, then the error will not cause any of the writer log (which has already been written) to be lost. it may cause some entries which have not yet been written to never be written, but nothing which has already been written will be lost

#### things you might use an error effect for
* uniform error handling behaviour across sync and async contexts

```clojure
(defn inc-it
  [v]
  (m/mlet [_ (writer/tell [:inc v])]
    (m/return (inc v))))

(-> (m/return 100)
    (m/bind inc-it)
    ;; this throw will not affect the logged output
    (m/bind (fn [v] (throw (ex-info "boo" {:v v}))))
    (err/catch (fn [e] (m/return (-> e ex-data :v))))
    (r/run {}))

;; =>
;; {:promise/val 101
;;  :promise.writer [[:inc 100]]}
```

### reader effect

the reader effect allows functions to access a shared environment without having to add it to every function signature

#### things you might put in a reader environment:
* http clients, db clients and other IOC dependencies
* a log context such as a transaction id

```clojure
(defn fetch-widget
  [id]
  (m/mlet [http (reader/asks :http/client)
           :let [url (str "https://foo.com/widgets/" id)]]
    (GET http url)))

(defn save-widget
  [widget]
  (m/mlet [db (reader/asks :db/client)]
    (db/insert db widget)))


(-> (get-widget "foo")
    (save-widget)
    (r/run {:http/client <http-client>
            :db/client <db-client>}))

;; =>
;; {:promisefx/val <widget>}
```

### writer effect

the writer effect allows functions to write to an append-only log without needing to manage the logged values in function results

#### things you might use a writer effect for:
* logging
* accumulating re-frame like effect data

```clojure
(defn user-welcome-email
  [{email :email name :name :as user}]
  (m/mlet [_ (writer/tell [:send-email {:to email
                                        :subject "welcome"
                                        :body (str "hi " name)}])
    (m/return user])))

(defn user-db-record
  [user]
  (m/mlet [_ (writer/tell [:db-change [:users [nil user]]])
    (m/return user)]))

(defn create-user
  [user]
  (m/mlet [_ (user-welcome-email user)
           _ (user-db-record user)]
    (m/return user)))


(-> (m/return {:id 100 :email "foo@foo.com" :name "mr. foo mcfoo"})
    (m/bind create-user)
    (r/run {}))

;; =>
;; {:promisefx/val {:id 100 :email "foo@foo.com" :name "mr. foo mcfoo"}
;;  :promisefx.writer/output {:send-email [{:to "foo@foo.com"
;;                                          :subject "welcome"
;;                                          :body "hi mr. foo mcfoo"}]
;;                            :db-change [:users [nil {:id 100
;;                                                     :email "foo@foo.com"
;;                                                     :name "mr. foo mcfoo"}]]}}
```

### state effect

the state effect allows functions to access and modify a state value without needing to account for it in function signatures or results

#### things you might use a state effect for:
* building an app-context map
* accumulating state in a multi-step process

```clojure
(defn fetch-user
  [id]
  (m/mlet [http (reader/asks :http/client)
           :let [url (str "https://foo.com/users/" id)]]
    (GET http url)))

(defn fetch-user-widgets
  [{id :id :as user}]
  (m/mlet [http (reader/asks :http/client)
           :let [url (str "https://foo.com/users/" id "/widgets")]]
    (GET http url)))

(defn fetch-user-and-widgets
  [user-id]
  (m/mlet [user (fetch-user user-id)
           _ (state/swap assoc :user user)
           widgets (fetch-user-widgets user)
           _ (state/swap assoc :widgets widgets)]
     (state/get)))

(-> (m/return 100)
    (m/bind fetch-user-and-widets)
    (r/run {:http/client <http-client>>}))

;;=>
;; {:user {:id 100 :name "mr. foo mcfoo"
;;  :widgets [{:user_id 100 :widget_id 1 :name "poker"}
;;            {:user_id 100 :widget_id 2 :name "scraper"}]}}
```

## contexts

these effects are made available in the following contexts:

* exception - [error] - synchronous execution
* promise - [error] - asynchronous execution
* RWSX - [error, reader, writer, state] - synchronous execution
* RWSPromise - [error, reader, writer, state] - asynchronous execution

### exception context

* values in the exception context are either Success or Failure records
* any exception thrown will be caught and wrapped in a Failure

### promise context

* values in the promise context are promises
* any exception thrown will be caught and wrapped in a failed promise

### RWSX context

* values in the RWSX context are functions of the environment+state, returning writer output, and updated state and a value as a map

```clojure
{:promisefx/val <value>
 :promisefx.writer/output <output>
 :promisefx.state/state <updated-state>}
```
* any exception thrown will be caught and wrapped in a Failure

### RWSPromise context

* values in the RWSPromise context are functions of the environment+state, returning a promise of writer output, updated state and a value,

```clojure
Promise<{:promisefx/val <value>
         :promisefx.writer/output <output>
         :promisefx.state/state <updated-state>}>
```
* any exception throw will be caught and wrapped in a failed promise

## Usage


## License

Copyright © 2020-2021 mccraigmccraig

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
