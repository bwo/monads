# monads

Yet another clojure library for monads.

## Usage

Never before has performing analogues of addition been more exciting:

```clojure
user> (run-reader-t (reader-t maybe-m) (mplus mzero (asks inc)) 4)
#<Just 5>
```

## License

Copyright © 2013 Ben Wolfson 

Distributed under the Eclipse Public License, the same as Clojure.
