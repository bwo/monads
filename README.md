# monads

Yet another clojure library for monads.

The primary goals for this library are expressivity and correctness. Things
should do what they say on the tin, and it should not be a pain to
construct complex monadic computations or use stacks of multiple
transformers. Monadic computations should be expressible generically
as far as is possible. Performance has been a secondary goal, but I
seem to be getting [good
results](https://github.com/bwo/monads/wiki/Tree-numbering-benchmark).

The idiom is unabashedly Haskell-derived: the bind function, for
instance, is spelled `>>=`, and the special syntax, `mdo`, apes
Haskell's do-notation far more closely than does `algo.monads`'
`domonad`. (A
[justification/rant](https://bitbucket.org/kenko/macroparser/src/eb372fec0e3a30daf7e8d946cacea4ceba86a0ea/src/macroparser/monads.clj?at=default#cl-7)
on this topic is available.)

There are some code examples and some benchmarking on the [wiki](https://github.com/bwo/monads/wiki).

Implementations are provided for reader, list, maybe, identity,
continuation, state, and error monads, and with transformers for all
except list (since the straightforward list-t implementation does not
always yield a true monad, though I may yet implement
[logic-t](http://hackage.haskell.org/package/logict)) and identity.

A caveat: use of the "bare" monads (maybe-m, error-m, etc.) is
vulnerable to stack-blowing on deeply nested computations, e.g. `(msum
(repeat 4000 mzero))`. This danger can be *mostly* obviated by using
the transformer version of the monad with cont-m as the base monad:

```clojure
monads.maybe> (run-monad maybe-m (u/msum (repeat 4000 mzero)))
; Evaluation aborted.
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (u/msum (repeat 4000 mzero))))
nil
```

However, this doesn't get around the entire problem: msum is written
to associate to the right. A left-associative version would still blow
the stack:

```clojure
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (reduce mplus mzero (repeat 4000 mzero))))
; Evaluation aborted.
nil
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (reduce #(mplus %2 %1) mzero (reverse (repeat 4000 mzero)))))
nil
```

There is a [branch](https://github.com/bwo/monads/tree/tramp) that
attempts to avoid this by essentially making, by hand, every monad a
monad transformer transforming a trampolining continuation monad;
however, this approach has several disadvantages: in particular, it
slows everything down and makes the code more complicated---especially
the code for the list monad, which has a natural transformation which
is not lazy, a more complicated translation which is kind of lazy but
can't be properly lifted into a monad tranformer, and a slightly more
complicated yet translation which can be lifted but is even less lazy.

## Usage

Monadic computations are run using `run-monad`, or, for the reader,
state, and continuation monads, special `run-{reader,state,cont}{,-t}`
functions that accept additional parameters and unwrap the results
(usually just by applying a function).

Monads themselves are just maps; at a minimum they should have a
`:bind` key giving the implementation of `>>=` and a `:return` key
giving the implementation of `return`. There are `defmonad` and
`monad` macros that check this. Monad transformers are implented as
functions whose arguments are the monads to be transformed; the
returned map should additionally have an `:inner` key whose value is
the monad "one layer down" in the stack.

All the monad implementations live in same-named namespaces, e.g.
`monads.reader` contains `reader-m` and `reader-t`. (The monads and
the transformers also have single-letter aliases; given a `(require
[monads.reader :as reader])`, one may refer to the transformer as
`reader/t` rather than `reader/reader-t`).

All monads support the basic `>>=` and `return` operations; all
transformers additionally support `lift`. Additional operations are
supported by only some monads (suffixless names are given in this
list; the base monads are formed by suffixing `-m`, the transformers
by suffixing `-t`):

- `mfail`: supported by `maybe` and `error`. Any monad transformer
   transforming a monad that supports these also supports them.
   the result of any transformer whose argument supports it.
- `mzero`, `mplus` (i.e., Haskell's MonadPlus typeclass): supported by
   `maybe`, `error`, and `list`. Any monad transformer transforming a
   monad that supports these also supports them.
- `get-state`, `put-state`, `modify`: supported by `state`.
- `throw-error`, `catch-error`: supported by `error`.
- `callcc`: supported by `cont`.
- `ask`, `asks`, `local`: supported by `reader`.
- `tell`, `pass`, `listen`, `listens`, `censor`: supported by
   `writer`.

There are also some generic functions defined in terms of the above
(in fact some of the above are actually not primitive; for instance,
`modify` is defined in terms of `get-state` and `put-state`):

- `(sequence-m ms)`: transform a sequence of monadic actions into a
    monadic action yielding a sequence. (That is, go from `[m a]` to
    `m [a]`.)
- `(mwhen p m)`: execute monadic computation `m` if `p` is truthy.
- `(guard p)`: exit from the computation if `p` is falsy (requires
   `mzero`).
- `(lift-m f [m])`: lift the unary function `f` to be
    monadic, returning the monadic function with one arg or applying it
    immediately with two.
- `(lift-m-2 f [m [m2]])`: as `lift-m` but for binary functions.
- `(lift-m* f [& args])`: as `lift-m` but for arbitrary arities.
- `ap`: lifts function application, but only for curried functions:

   ```clojure
   (run-monad maybe-m (ap (ap (return (curryfn #(+ %1 %2))) (return 1)) (return 2)))
   #<Just 3>
   ```

   `lift-m*` is likelier to be useful.
- `(fold-m f init xs)`: apply a reduction within a monad. NB: the
   arguments here  are as in Haskell's `foldM`, and *not* as in
   `algo.monads`' `m-reduce`! `fold-m` expects `f` to have type `a ->
   b -> m a`, `init` to have type `a`, and `xs` to have type `[b]`,
   whereas `m-reduce` expects `f` to have type `a -> b -> a`, `init`
   to have  type `a`, and `xs` to have type `[m b]`.

Further such functions are easily defined (and I intend to add
analogues for many of those in the Haskell Prelude). This, for
instance, is the definition of `guard`:

```clojure
(defn guard [p]
  (if p
    (return nil)
    mzero))
```

Never before has performing analogues of addition been more exciting:

```clojure
user> (run-reader-t (reader-t maybe-m) (mplus mzero (asks inc)) 4)
#<Just 5>
user> ((run-state-t (state-t (reader-t maybe-m)) (mplus mzero (lift (asks inc))) 4) 1)
#<Just #<Pair [2 4]>>
user> ((eval-state-t (state-t (reader-t list-m)) (mplus get-state (lift (asks inc))) 4) 1)
(4 2)
```

## License

Copyright © 2013 Ben Wolfson 

Distributed under the Eclipse Public License, the same as Clojure.
