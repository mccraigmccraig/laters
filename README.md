# promisefx

[![cljdoc badge](https://cljdoc.org/badge/mccraigmccraig/promisefx)](https://cljdoc.org/d/mccraigmccraig/promisefx/CURRENT)
![mccraigmccraig/promisefx](https://github.com/mccraigmccraig/promisefx/.github/workflows/test.yml/badge.svg)

an exploration of some other ideas for effectful programming with
clojure/script

effectful programming offers some useful ways of separating the what
of some code concerns from the how

promisefx came into being while working with a promise-based asynchronous system,
because some useful things from synchronous environments (e.g. stacktraces,
thread-local context variables) are not available in asynchronous environments,
making debugging, logging and tracing all harder

promisefx implements some effects which can solve these problems without being
dependent on any features of the execution environment beyond closures.
it brings the following effects:

* error - uniform error handling in different execution contexts
* reader - an input-only effect
* writer - an output-only effect
* state - a modifiable state effect ()i'm not even sure this should be here, but it's approximately free given the implementations of the other effects, so why not)

### error effect

the error effect provides uniform error behaviour in different contexts without impacting other effects. if an error is thrown, then the current computation will short-circuit until it either ends or the error is caught. if a vanilla exception is thrown in a context with an error effect then the exception is wrapped in a Failure and will behave as if an error-effect error were thrown

the error effect does not impact other effects. e.g. if an error is thrown in a computation which also has a writer effect, then the error will not cause any of the writer log (which has already been written) to be lost. it may cause some entries which have not yet been written to never be written, but nothing which has already been written will be lost

#### interface

* `(throw e)` thow an error - you can also use a regular clojure/script `throw`
* `(catch (fn [e] ...))` catch an error - you can't (in general) use a regular clojure `catch` (e.g. because vanilla `catch` doesn't work in a promise-based context)
* `(handle (fn [failure success] ...))` handle both sucess and failure with a 2-arity handler

#### things you might use an error effect for
* helping your code to focus on its domain and be independent of the execution context

e.g. this example will work unchanged in either of the synchronous RWSX or asynchronous RWSPromise contexts

```clojure
(defn inc-it
  [v]
  (m/mlet [_ (writer/tell [:inc v])]
    (m/return (inc v))))

(-> (inc-it 100)
    ;; this throw will not affect the logged output
    (m/bind (fn [v] (throw (ex-info "boo" {:v v}))))
    (err/catch (fn [e] (m/return (-> e ex-data :v))))
    (r/run {}))

;; =>
;; {:promisefx/val 101
;;  :promisefx.writer/output [[:inc 100]]}
```

### reader effect

the reader effect allows functions to access a shared environment without having to add it to every function signature

#### interface

* `(ask)` return the environment value
* `(asks f)` return the environment value modified by `f`
* `(local f mv)`  run `mv` with an environment modified by `f`

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
    (m/bind save-widget)
    (r/run {:http/client <http-client>
            :db/client <db-client>}))

;; =>
;; {:promisefx/val <widget>}
```

### writer effect

the writer effect allows functions to write to an append-only log without needing to manage the logged values in function results

#### interface

* `(tell v)` write the value
* `(listen mv)` execute `mv` and return a pair of `[value output]``
* `(pass mv)`

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


(-> (create-user {:id 100 :email "foo@foo.com" :name "mr. foo mcfoo"})
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

#### interface

* `(get)` get the state value
* `(put)` put the state value
* `(gets f)` get the state value and apply f
* `(swap f)` update the state value with f, returing the old value

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

(-> (fetch-user-and-widets 100)
    (r/run {:http/client <http-client>>}))

;;=>
;; {:promisefx/val {:user {:id 100 :name "mr. foo mcfoo"
;;                  :widgets [{:user_id 100 :widget_id 1 :name "poker"}
;;                            {:user_id 100 :widget_id 2 :name "scraper"}]}}
;;  :promisefx.state/state {:user {:id 100 :name "mr. foo mcfoo"
;;                          :widgets [{:user_id 100 :widget_id 1 :name "poker"}
;;                                    {:user_id 100 :widget_id 2 :name "scraper"}]}}}
```

## contexts

these effects are made available in the following contexts:

* exception - [error] - synchronous execution with an error effect
* promise - [error] - asynchronous execution with an error effect
* RWSX - [error, reader, writer, state] - synchronous execution with multiple effects
* RWSPromise - [error, reader, writer, state] - asynchronous execution with multiple effects

### exception context

* values in the exception context are either Success or Failure records
* any exception thrown will be caught and wrapped in a Failure
* this is the default context for any value which doesn't otherwise identify a context

### promise context

* values in the promise context are promises
* any exception thrown will be caught and wrapped in a failed promise
* this is the default context for promises and deferreds

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

## Lifting

all of the above contexts will attempt to lift any value returned to them by a bind function - this may not always
be successful


## License

Copyright © 2020-2021 mccraigmccraig

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
