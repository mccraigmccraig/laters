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

the error effect provides uniform error behaviour in different contexts without impacting other effects. if an error is thrown, then the current computation will short-circuit until it either ends or the error is caught

the error effect does not impact any other effects. e.g. if an error is thrown in a computation which also has a writer effect, then the error will not cause any of the writer log (which has already been written) to be lost. it may cause some entries which have not yet been written to never be written, but nothing which has already been written will be lost

```clojure
(defn inc-it
  [v]
  (m/mlet [_ (w/tell [:op :inc])]
    (m/return (inc v))))

(-> (m/return 100)
    (m/bind inc-it)
    ;; this throw will not affect the logged output
    (m/bind (fn [v] (throw (ex-info "boo" {:v v}))))
    (err/catch (fn [e] (m/return (-> e ex-data :v)))))
```

### reader effect

the reader effect allows functions to access a shared environment without having to add it to every function signature

### writer effect

the writer effect allows functions to write to an append-only log without needing to manage the logged values in function results

### state effect

the state effect allows functions to access and modify a state value without needing to account for it in function signatures or results

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

```
{:promisefx/val <value>
 :promisefx.writer/output <output>
 :promisefx.state/state <updated-state>}
```
* any exception thrown will be caught and wrapped in a Failure

### RWSPromise context

* values in the RWSPromise context are functions of the environment+state, returning a promise of writer output, updated state and a value,

```
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
