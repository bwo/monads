# monads

Yet another clojure library for monads, focussing on expressivity and correctness.

For Leiningen:

```clojure
[bwo/monads "0.2.2"]
```

The idioms and terminology for this library are unabashedly
Haskell-derived: there is a special syntax for monad computations,
`mdo`, which is similar to Haskell's do-notation, and the names (and
selection) of monads which have implementations provided out of the
box are influenced by the mtl.

## Improvements from 0.1.0

- All monad implementations interoperate with
  `clojure.algo.generic.functor`.

- Internals rewritten to be faster and more flexible.

- Automatic lifting in monad transformers

- Applicative functors introduced; all monads support its interface.

- Combined reader/writer/state monad implementation

## Usage

There are some code examples, and some benchmarks, on the
[wiki](https://github.com/bwo/monads/wiki); the examples show building
up a simple expression evaluator.

Monadic computations are built up using `return` and `>>=`. For
instance, one could define `lift-m-2` (which enables the application
of a function to monadic values) as follows:

```clojure
(defn lift-m-2
  "Take a function a -> b -> c and two values m a and m b, and return
  m c."
  [f m1 m2]
  (>>= m1 (fn [v1] (>>= m2 (fn [v2] (return (f v1 v2)))))))
```

Since writing functions this way is cumbersome, a macro is provided
that mimics Haskell's do-notation:

```clojure
(defn lift-m-2
  "Take a function a -> b -> c and two values m a and m b, and return
  m c."
  [f m1 m2]
  (mdo v1 <- m1
       v2 <- m2
       (return (f v1 v2))))
```

(The actual implementation of `lift-m-2` in `monads.util` is slightly
different again, due to being curried.)

However, with *only* `return` and `>>=`, we can't do anything that we
couldn't do with ordinary functions. There are also several protocols
that specific monads can implement, which bring with them specific
operations allowing more interesting things. Monad transformers can be
used to conveniently add capabilities together.

Implementations are provided for several monads:

|Monad       |Transfomer provided? |Example use case    |Protocols supported      |Specific operations      |
|------------|----------|----------------|---------------|---------------|
|reader      |yes       |read-only access to global environment |monadreader    |`ask`, `local`          |
|state       |yes       |simulate mutable state       |monadstate     |`get-state` , `put-state`, `modify`   |
|writer      |yes       |log messages during a computation    |monadwriter    |`tell`, `pass`, `listen`, `listens`, `censor`         |
|maybe       |yes       |computations may fail   |monadfail, monadplus     |`fail`, `mzero`, `mplus`         |
|error       |yes       |computations that may fail, error handling and recovery   |monadfail, monadplus, monaderror     |`fail`, `mzero`, `mplus`, `throw-error`, `catch-error`         |
|list        |no        |computations that may produce multiple results   |monadfail, monadplus     |`fail`, `mzero`, `mplus`         |
|cont        |yes       |arbitrary manipulation of control, emulate CPS transform       |(none---not yet abstracted out) |`callcc`, `shift`, `reset`        |
|rws         |yes       |inline combination of reader, writer, and state          |monadstate, monadwriter, monadreader    |union of reader, state, writer     |
|identity    |yes       |trivial monad   |(none)         |None           |

The specific operations are documented in
[`monads.core`](https://github.com/bwo/monads/blob/master/src/monads/core.clj),
or for the continuation operations, in
[`monads.cont`](https://github.com/bwo/monads/blob/master/src/monads/cont.clj).

The protocols are defined in `monads.types` and the functions to take
advantage of them are defined in `monads.core` (with the exception of
`shift` and `reset`, which are defined in `monads.cont`).

The "base" monads are named by suffixing `-m` to the names in the
table above (e.g. `state-m`, `cont-m`). If there is a transformer
version of a monad, it is a function named by suffixing `-t` instead
of `-m`. The monad and transformer implementations are found in
namespaces given by the names in the table, so, e.g., `state-m` and
`state-t` are defined in `monads.state`. Each such namespace also
defines vars named `m` and `t` as shortcuts, so you can refer to
`state/m` instead of stuttering out `state/state-m`.

Giving the transformer function a monad as an argument returns a new
monad. The resulting "monad transformer stack" implements the
MonadTrans protocol and supports two additional operation, `lift` and
`inner`. `inner` returns the monad that was originally passed in as an
argument; `lift` can be used to run operations specific to a base
monad in the stack. In general, explicit lifting is not necessary with
the monads and transformers defined in this library, as the
transformers will automatically support the operations their arguments
do. Explicit lifting is only necessary for disambiguation if more than
one monad supports the same operation:

```clojure
monads.core> (require '[monads.state :as st] '[monads.error :as e] '[monads.maybe :as m])
nil
;; the next two lines are equivalent, because `state-t` will auto-lift `fail`
monads.core> (st/run-state-t (st/t e/m) (lift (fail "oops")) :initial-state)
#<Either [:left oops]>
monads.core> (st/run-state-t (st/t e/m) (fail "oops") :initial-state)
#<Either [:left oops]>
;; and the next two lines are also equivalent, because the auto-lifting goes down one level in the stack
monads.core> (st/run-state-t (st/t (e/t m/m)) (fail "oops") :initial-state)
#<Just #<Either [:left oops]>>
monads.core> (st/run-state-t (st/t (e/t m/m)) (lift (fail "oops")) :initial-state)
#<Just #<Either [:left oops]>>
;; so if we explicitly want to use `maybe-m`'s fail, we need to lift down from the bottom.
monads.core> (st/run-state-t (st/t (e/t m/m)) (lift (lift (fail "oops"))) :initial-state)
nil
```

In general, monadic computations are *run* using `run-monad`, which
takes two arguments: a monad and a monadic computation. However, as
the above example, using `run-state-t`, suggests, there are helper
functions for some specific monads (any of those that require extra
initial data):

|Monad        |Run function                           |Extra  arguments     |
|-------------|-------------------------------|------------|
|`state-{m,t}` |`monads.state/run-state{,-t}`  |Initial state    |
|`reader-{m,t}`|`monads.reader/run-reader{,-t}`|Starting environment   |
|`cont-{m,t}`  |`monads.reader/run-cont{,-t`}  |None*      |
|`rws-{m,t}`   |`monads.rws/run-rws{,-t}`      |Initial state and starting argument    |

(* In principle the extra argument should be the final continuation,
but this is actually chosen by the implementation to be `return` for
`cont-t` and `identity` for `cont-m`.)

`run-state`, `run-reader`, `run-cont`, and `run-rws` do not need the
monad passed as their first argument, since it is assumed that the
computation should be run in the `state`, `reader`, `cont`, or `rws`
monads, respectively.

## Utility functions

The function `lift-m`, which lifts a function defined over types `a ->
b` to one defined over types `m a -> m b` for any monad m, is provided
in monads.core; importing this file also makes all monads correctly
treat the `fmap` defined in algo.generic correctly.

A (not very systematic) selection of monad functions is provided in
`monads.util`:

- `(sequence-m ms)`: transform a sequence of monadic actions into a
    monadic action yielding a sequence. (That is, go from `[m a]` to
    `m [a]`.)
- `(mwhen p m)`: execute monadic computation `m` if `p` is truthy.
- `(guard p)`: exit from the computation if `p` is falsy (requires
   `mzero`).
- `(lift-m-2 f [m [m2]])`: as `lift-m` but for binary functions.
   
    There are also `lift-m-3` through `lift-m-8`. All the `lift-m-n`
    functions are fully curried and can take at any stage anywhere
    from one to the remaining number of arguments, e.g. `((lift-m-3 +
    a b) c)`, `(((lift-m-3 +) a) b c)`, etc. In the unlikely event
    that a lifting function of yet greater arity is needed, the
    `deflift-m-n` macro can be used to create one. `deflift-m-ns` can
    be used to create a range of such functions.

- `(lift-m* f [& args])`: as `lift-m` but for arbitrary arities. (N.B.
   this is implemented using sequence-m and each appears to behave
   unexpectedly in the context of the continuation monad's `shift` and
   `reset`, but those should probably be considered experimental for
   the time being).
- `ap`: lifts function application, but only for curried functions:

   ```clojure
   (run-monad maybe-m (ap (ap (return (curryfn [a b] (+ a b))) (return 1)) (return 2)))
   #<Just 3>
   ```

   `lift-m*` is likelier to be useful, unless you happen to have a lot
   of curried functions lying around.
- `(fold-m f init xs)`: apply a reduction within a monad. NB: the
   arguments here  are as in Haskell's `foldM`, and *not* as in
   `algo.monads`' `m-reduce`! `fold-m` expects `f` to have type `a ->
   b -> m a`, `init` to have type `a`, and `xs` to have type `[b]`,
   whereas `m-reduce` expects `f` to have type `a -> b -> a`, `init`
   to have  type `a`, and `xs` to have type `[m b]`.
- `(msum [...])` "adds" the elements of its argument list with `mplus`.

Further such functions are easily defined. This, for instance, is the
definition of `guard`:

```clojure
(defn guard [p]
  (if p
    (return nil)
    mzero))
```

These are just ordinary Clojure functions that need not know anything
about the context in which they will eventually be used.

## Special syntax

While it is perfectly possible to write monadic computations as chains
of `>>=` and anonymous functions, this quickly becomes tedious; a
macro, `mdo`, is provided to make things simpler. As noted above, the
syntax is very much derived from Haskell.

There are three types of elements of an `mdo` form:

- *binding* elements, which have the form `destructure <- expression`;

- *plain* elements, which are just expressions (except that no such
   expression can consist solely of the symbol `<-` or the symbol
   `let`);

- *let* elements, which have the form `let destructure = expression`
   (or `let destructure1 = expression1, destructure2 = expression2,
   ...`. The commas here are just for presentation; since the reader
   gobbles them up, they aren't (and can't be) necessary to the
   syntax)

   let elements may also be written with a more conventional binding
   vector: `let [destructure expression ...]`.

The final element of an `mdo` form must be a plain element.

In the above `destructure` can be any valid Clojure binding form. The
expression on the left-hand side of a binding element, and the
expression in a plain element, should have a monadic value; these are
unwrapped and bound to the binding form on the right-hand side of the
binding element, if there is one. Bindings established with `let`
forms are, by contrast, pure (or at least treated as pure). Both forms
of bindings are visible in all following statements (if not shadowed,
of course).

So the following, for instance, is a not very interesting computation
in the state monad:

```clojure
(mdo {:keys [x y]} <- get-state
     let [z (+ (* x x) (* y y))]
     (modify #(assoc % :z z))
     (return z)
```

It does what you would expect:

```clojure
> (def m (mdo {:keys [x y]} <- get-state
              let z = (+ (* x x) (* y y))
              (modify #(assoc % :z z))
              (return z)))
> (run-state m {:x 1 :y 3})
#<Pair [10 {:z 10, :y 3, :x 1}]>
> (run-state-t (state-t monads.maybe/maybe-m) m {:x 1 :y 3})
#<Just #<Pair [10 {:z 10, :y 3, :x 1}]>>

```

And expands into uses of `>>=` and anonymous functions:

```clojure
(>>=
 get-state
 (fn [{:keys (x y)}]
     (let [z (+ (* x x) (* y y))]
       (>>= (modify #(assoc % :z z)) (fn [G__6125] (return z))))))
```

In fact, the "let" form is not really necessary; we could have omitted
it and simply written this:

```clojure
(mdo {:keys [x y]} <- get-state
     (let [z (+ (* x x) (* y y))]
       (mdo (modify #(assoc % :z z))
            (return z))))
```

And only suffered a little indentation. Similarly, there is no need
for special syntax for `if` or `when` (and none is provided); just as
we can write this code:

```clojure
monads.list> (def pythags (mdo a <- (range 1 200)
                               b <- (range (inc a) 200)
                               let a2+b2 = (+ (* a a) (* b b))
                               c <- (range 1 200)
                               (monads.util/guard (== (* c c) a2+b2))
                               (return (list a b c))))
#'monads.list/pythags
monads.list> (take 3 (run-monad list-m pythags))
((3 4 5) (5 12 13) (6 8 10))
```

We could have taken advantage of the fact that the return is the only
statement following the guard:

```clojure
monads.list> (def pythags (mdo a <- (range 1 200)
                               b <- (range (inc a) 200)
                               let a2+b2 = (+ (* a a) (* b b))
                               c <- (range 1 200)
                               (if (== (* c c) a2+b2)
                                   (return (list a b c))
                                   mzero)
```

## Applicative functors

`monads.applicative` defines a simple applicative functor interface,
and gives default implementations for it to all monads, as well as for
sequences, nil, the `Just` and `Either` types defined in
`monads.types`, and `Const` and `Id` functors also defined in
`monads.applicative`.

The applicative interface consists of `pure`, which is analogous to
`return` for monads, and effectful function application, `<*>`. Since
we don't assume that all arguments will be supplied immediately,
however, the function argument to `<*>` must be curried, so that
arguments can be fed in one by one. A convenience function `cpure` is
supplied that takes an arity and a function and returns a curried
function with the given arity wrapped in the Pure constructor:

```clojure
monads.applicative> (require '[monads.types :as t])
nil
monads.applicative> (<*> (cpure 3 +) (t/just 3) (t/just 1) (t/just 2))
#<Just 6>
monads.applicative> (<*> (cpure 3 +) (t/just 3) t/nothing (t/just 2))
nil
monads.applicative> (<*> (<*> (cpure 3 +) (t/just 3)) (t/just 1) (t/just 2))
#<Just 6>
```

General utilities for currying functions can be found in
`monads.util`: the macros `curryfn` and `defcurryfn` define curried
functions, and the macro `curry` and function `ecurry` both take an
arity and a function and create a curried function with the given
arity. `curry` falls back to `ecurry` if the arity is not statically
known; if it is known, `curry` is significantly faster:

```clojure
monads.util> (time (dotimes [_ 10000] ((((ecurry 3 +) 1) 2) 3)))
"Elapsed time: 30.518729 msecs"
nil
monads.util> (time (dotimes [_ 10000] ((((curry 3 +) 1) 2) 3)))
"Elapsed time: 7.261895 msecs"
nil
```

Despite the inconvenience of manual currying, applicative functors are still useful; as an example, [`monads.examples.applicative-fold`](https://github.com/bwo/monads/blob/master/src/monads/examples/applicative_fold.clj) contains an implementation of the core of [a streaming fold abstraction](http://www.haskellforall.com/2013/08/composable-streaming-folds.html) (though for complex computations something like [babbage](https://github.com/readyforzero/babbage) might be better).

## Implementation

Monads are implemented with a protocol defining a binary `mreturn` and
trinary `bind` operations; the additional parameter over `return` and
`>>=` is for the carrier of the protocol. There are `monad` and
`defmonad` macros which delegate to `reify`; the followuing
definitions of the identity monad are equivalent:

```clojure
(defmonad identity-m
  (mreturn [me x] x)
  (bind [me m f] (run-monad me (f m))))

(require '[monads.types :as types])
(def identity-m 
    (reify types/Monad
       (mreturn [me x] x)
       (bind [me m f] (run-monad me (f m)))))
```

However, `monad` and `defmonad` allow one to conditionally support
other protocols as well, which is useful for defining monad
transformers that support a protocol if the transformed, inner monad
does.

Since the macros know that they are defining a monad, nothing special
needs to be done to ensure that `mreturn` and `bind` find their homes
in the right protocol; other protocols need to be given explicitly
using `reify`-like syntax. For example, the `reader-t` transformer
function looks like this:

```clojure
(defn reader-t [inner]
  (monad
   (mreturn [me v] (constantly (types/mreturn inner v)))
   (bind [me m f] (fn [e]
                    (run-mdo inner
                             a <- (m e)
                             (run-reader-t me (f a) e))))
   types/MonadTrans
   (inner [me] inner)
   (lift [me c] (fn [e] (run-monad inner c)))
   types/MonadReader
   (ask [me] (fn [e] (types/mreturn inner e)))
   (local [me f m] (fn [e] (run-reader-t me m (f e))))
   (when (types/monadfail? inner)
     types/MonadFail
     (fail [me msg] (fn [e] (types/fail inner msg))))
   (when (types/monadplus? inner)
     types/MonadPlus
     (mzero [me] (constantly (types/mzero inner)))
     (mplus [me lr] (fn [e]
                      (types/mplus inner
                                   (lazy-pair (run-reader-t me (first lr) e)
                                              (run-reader-t me (second lr) e))))))))
```
Note the conditional support for `MonadFail` and `MonadPlus`.

## A caveat about the stack

The use of the "bare" monads (maybe-m, error-m, etc.) is vulnerable to
stack-blowing on deeply nested computations, e.g. `(msum (repeat 4000
mzero))`. This danger can be *mostly* obviated by using the
transformer version of the monad with cont-m as the base monad:

```clojure
monads.maybe> (require '[monads.util :as u] '[monads.cont :as c])
nil
monads.maybe> (run-monad maybe-m (u/msum (repeat 4000 mzero)))
; Evaluation aborted.
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (u/msum (repeat 4000 mzero))))
nil
```

On a less trivial computation:

```clojure
monads.examples.treenumber> (require '[monads.cont :as c])
nil
monads.examples.treenumber> (def x (num-tree (longtree 10000)))
StackOverflowError   monads.core/fn--1769 (core.clj:63)
monads.examples.treenumber> (def x (c/run-cont (s/run-state-t (s/t c/m) (number-tree (longtree 10000)) {})))
#'monads.examples.treenumber/x
monads.examples.treenumber> (count (second x)) ;; check that we've actually got the right # of entries
10000
```

However, this doesn't get around the entire problem: msum is written
to associate to the right. A left-associative version would still blow
the stack:

```clojure
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (reduce mplus mzero (repeat 4000 mzero))))
; Evaluation aborted.
```

The same thing happens with nested binds on the left:

```clojure
monads.maybe> (monads.cont/run-cont (run-monad (t monads.cont/m)
                (reduce (fn [acc _] (>>= acc (fn [x] (return (inc x))))) 
                        (return 0)
                        (range 10000))))
StackOverflowError   monads.types.Bind (types.clj:33)
```

However, since we have a programmatically manipulable representation
of the computation, this difficulty can be worked around:

```clojure
monads.maybe> (monads.cont/run-cont (run-monad (t monads.cont/m) 
                (monads.cont/reorganize (reduce (fn [acc _] (>>= acc (fn [x] (return (inc x))))) 
                                                (return 0) 
                                                (range 10000)))))
#<Just 10000>
monads.maybe> (monads.cont/run-cont (run-monad (t monads.cont/m)
                (monads.cont/reorganize (reduce #(mplus %1 %2)
                                                mzero
                                                (repeat 10000 mzero)))))
nil
```

Monadic computations are required to ensure the behavioral identity of
`(>>= (>>= m f) g)` and `(>>= m (fn [x] (>>= (f x) g)))`, so the
`reorganize` function can convert left-biased computations with the
former shape to right-biased computations with the latter. Since
`mplus` is similarly required to be associative, it does the same for
left-biased `mplus` applications, rewriting `(mplus (mplus a b) c)` to
`(mplus a (mplus b c))`.

Note that this reorganization at present doesn't descend into the
monadic arguments of e.g. `local`, and (obviously) the contents of
closures in the second argument of `>>=` are opaque to it. If the
rewriting were baked into `mplus` and `>>=`, this would not be an
issue, but I'm hesitant to carry the rewriting out if it's not asked
for.

## License

Copyright © 2014 Ben Wolfson 

Distributed under the Eclipse Public License, the same as Clojure.
