# monads

Yet another clojure library for monads. In progress.

## Usage

Never before has performing analogues of addition been more exciting:

```clojure
user> (run-reader-t (reader-t maybe-m) (mplus mzero (asks inc)) 4)
#<Just 5>
user> ((run-state-t (state-t (reader-t maybe-m)) (mplus mzero (lift (asks inc))) 4) 1)
#<Just #<Pair [2 4]>>
user> ((run-state-t (state-t (reader-t list-m)) (mplus get-state (lift (asks inc))) 4) 1)
(#<Pair [4 4]> #<Pair [2 4]>)
```

## License

Copyright © 2013 Ben Wolfson 

Distributed under the Eclipse Public License, the same as Clojure.
