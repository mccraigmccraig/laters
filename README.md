# promisefx

an exploration of some other ideas for effectful programming with
clojure/script

## Usage

promises are nice for async programming, but they lose you some of
the niceties of blocking - stacktraces, threadlocals

there are other effects which some of the other functional languages
take for granted which we might want to use too

```clojure

```


## License

Copyright © 2020-2021 mccraigmccraig

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.



promisefx
         .context
                 .lifter
         .data
              .extractable
              .runnable
              .exception ... success / failure
              .tagged
              .monoid
         .fx
            .core ... bind/return/mlet
            .error ... throw/reject/catch/handle
            .reader
            .writer
            .state
         .tagged-ctx
         .identity-ctx
         .promise-ctx
         .exception-ctx
         .PRWS-ctx
         .RWSX-ctx
