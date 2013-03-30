# monads

Yet another clojure library for monads.

The primary goals for this library are expressivity and correctness.
Things should do what they say on the tin, and it should not be a pain
to construct complex monadic computations or use stacks of multiple
transformers. Monadic computations should be expressible generically
as far as is possible. Performance has been a secondary goal, but I
seem to be getting [good
results](https://github.com/bwo/monads/wiki/Tree-numbering-benchmark).
(Regarding correctness it should be noted that the list transformer is
modelled on the one in the Haskell transformers library and is hence
[subject to these
criticisms](http://www.haskell.org/haskellwiki/ListT_done_right).
Given that CLojure is not pervasively lazy, I'm not sure what a good
way to avoid the over-strictness is.)

The idiom is unabashedly Haskell-derived: the bind function, for
instance, is spelled `>>=`, and the special syntax, `mdo`, apes
Haskell's do-notation far more closely than does `algo.monads`'
`domonad`. 

There are some code examples and some benchmarking on the [wiki](https://github.com/bwo/monads/wiki).

Implementations are provided for reader, list, maybe, identity,
continuation, state, and error monads, with transformers for all
(though be aware that the list-t implementation does not always yield
a true monad).

## Library organization

Everything necessary to build up monadic computations is defined in
`monads.core`: the basic functions are `return`, to inject a value
into a monad, and `>>=`, to chain a monadic value with a function (as
in algo.monads' `m-bind`). There is also a convenience macro `mdo`
that makes expressing computations much simpler.

Several additional derived utility functions (`lift-m`, `guard`,
`fold-m`, etc.) live in `monads.util`. Individual monads and their
transformers are all in their own namespaces: `error-m` and `error-t`
live in `monads.error`, etc. Each such namespace also defines a
single-letter alias for the monad and transformer, `m` and `t`, so
that if you require `[monads.error :as error]` you can then refer to
the monad simply as `error/m` rather than as `error/error-m`.

Several of the monads (state, error, writer, and maybe) return custom
types whose accessors and constructors live in `monads.types`.

## Usage

Monadic computations are run using `run-monad`. Its first argument is
the monad to use; its second is the computation to run.

The reader, state, and continuation monads return do not immediately
return the value actually of interest; they have special
`run-{reader,state,cont}{,-t}` functions which should be used instead.
For `run-{reader,state,cont}`, since the monad is already known, it
does not need to be passed. The run functions for the reader and state
monads also have an additional parameter, representing the environment
and initial state, respectively.

All monads support the basic `>>=` and `return` operations; all
transformers additionally support the `lift` operation that lifts a
computation in the base monad into the monad transformer.

Additional operations are supported only by some monads: 

- `mfail`: supported by `maybe` and `error`. Any monad transformer
   transforming a monad that supports these also supports them.
   the result of any transformer whose argument supports it.
- `mzero`, `mplus`: supported by `maybe`, `error`, and `list`. Any
   monad transformer transforming a monad that supports these also
   supports them.
- `get-state`, `put-state`, `modify`: supported by `state`.
- `throw-error`, `catch-error`: supported by `error`.
- `callcc`: supported by `cont`.
- `ask`, `asks`, `local`: supported by `reader`.
- `tell`, `pass`, `listen`, `listens`, `censor`: supported by
   `writer`.

All of these except `callcc` are implemented in a monad-agnostic way:
it is possible to define additional monads that implement any of them
without changing the existing code.

## Utility functions

A (not very systematic) selection of monad functions is provided in
`monads.util`:

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
- `(lift-m* f [& args])`: as `lift-m` but for arbitrary arities. (N.B.
  this is implemented using sequence-m and appears to behave
  unexpectedly in the context of the continuation monad's `shift` and
  `reset`, but those should probably be considered experimental for
  the time being).
- `ap`: lifts function application, but only for curried functions:

   ```clojure
   (run-monad maybe-m (ap (ap (return (curryfn #(+ %1 %2))) (return 1)) (return 2)))
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
   syntax).

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
     let z = (+ (* x x) (* y y))
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

## Implementation

Monads are implemented as maps; there are `monad` and `defmonad`
macros, but all they do is allow a tiny bit of syntactic flexibility
and ensure that there are at least implementations of `>>=` and
`return`. The following definitions of the identity monad are equivalent:

```clojure
(defmonad identity-m
  :bind (fn [m f] (run-monad identity-m (f m)))
  :return identity)

(def identity-m {:bind (fn [m f] (run-monad identity-m (f m)))
                 :return identity})
```

The `>>=` and `return` operations simply package their operands up
into data structures which can then be interpreted by `run-monad`.

A monad supports the additional operations listed above by containing
additional nested maps. For example, the error monad looks like this:

```clojure
(let [mzero (left nil)]
  (defmonad error-m
    :return right
    :bind (fn [m f]
            (let [r (run-monad error-m m)]
              (either left #(run-monad error-m (f %)) r)))
    :monadfail {:mfail left}
    :monadplus {:mzero mzero
                :mplus (fn [lr]
                         (let [v (run-monad error-m (first lr))]
                           (if (left? v)
                             (run-monad error-m (second lr))
                             v)))}
    :monaderror {:throw-error left
                 :catch-error (fn [comp handler]
                                (let [v (run-monad error-m comp)]
                                  (either #(run-monad m (handler %)) right v)))}))
```

The values `mzero`, `mplus`, etc. defined in `monads.core`, in turn,
are or return values that, when run by run-monad, know how to look
themselves up in the monad and find their implementations. Thus, you
can provide a monad that implements `throw-error` and `catch-error`,
or `ask` and `local`, etc., without using the existing error or reader
monads. (For instance, you could define a read-write-state monad that
combines the operations of the reader, writer, and state monads in
one, without the overhead of lifting or excessive wrapping and
unwrapping.)

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
monads.maybe> (c/run-cont (run-monad (maybe-t c/m) (reduce #(mplus %2 %1) mzero (reverse (repeat 4000 mzero)))))
nil
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
                (reorganize (reduce (fn [acc _] (>>= acc (fn [x] (return (inc x))))) 
                                    (return 0) 
                                    (range 10000)))))
#<Just 10000>
```

Monadic computations are required to ensure the behavioral identity of
`(>>= (>>= m f) g)` and `(>>= m (fn [x] (>>= (f x) g)))`, the
`reorganize` function can convert left-biased computations with the
former shape to right-biased computations with the latter. (With a
small amount of work we could implement something similar for `mplus`,
which to the best of my knowledge is also required to be associative.
However, a [remark](http://okmij.org/ftp/continuations/Searches.hs) by
Oleg Kiselyov suggesting that associativity *isn't* always required
has led me to hold off on that for now.)

There is a [branch](https://github.com/bwo/monads/tree/tramp) that
attempts to avoid the necessity of using a transformer essentially by
translating every monad's implementation into CPS in a trampolining
continuation monad; however, this approach has several disadvantages:
in particular, it slows everything down and makes the code more
complicated---especially the code for the list monad, which has a
natural transformation which is not lazy, a more complicated
translation which is kind of lazy but can't be properly lifted into a
monad tranformer, and a slightly more complicated yet translation
which can be lifted but is even less lazy. (However, if anyone wants
to show me how to, or contribute code to, make the list monad lazy,
play nice with transformers, and be trampolined, I would be most
appreciative.)

## Why another monad implementation?

There are three other monad implementations for clojure that I know
of: [algo.monads](https://github.com/clojure/algo.monads),
[morph](https://github.com/blancas/morph), and Jim Duey's
[protocol-monads](https://github.com/jduey/protocol-monads/tree/master/src)
(which I haven't used). This implementation exists in part just
because I wanted to see if I could write a reasonably complete monad
implementation without using protocols (for reasons discussed below)
or algo.monads' symbol macros (because symbol macros get around one of
the problems with protocols, but seem otherwise inelegant given the
way they're implemented in Clojure), but partly in reaction to some
perceived flaws of the existing implementations (aside from
protocol-monads, which, as I mentioned, I haven't actually used):

- Both morph and algo.monads are incorrect.

  If `f` is a function `a -> m b`, then `(>>= (return a) f)` is
  supposed to be equivalent to `(f a)` for all `a`. morph breaks this
  for both its either and maybe monads; algo.monads for its maybe
  monad. (There is an
  [issue](http://dev.clojure.org/jira/browse/ALGOM-7) for this on the
  algo.monads JIRA, but AFAICT no action.)

  Additionally, algo.monads' continuation monad does not trampoline,
  which means it cannot be used to avoid overflowing the stack (though
  it should be admitted that my implementation overflows the stack
  sooner than algo.monads does). This is not, technically, incorrect,
  but I do think trampolining here is preferable.

- Protocol-based implementations are inexpressive.

  Consider the implementation of `guard` above. It would not be
  possible to write this using a protocol-based implementation: the
  sole argument is a boolean(-ish) value, so there is no way to select
  the right implementation of `return`, let alone the right value for
  `mzero`. One of the promises of monads is that code can be agnostic
  as to the eventual execution strategy, and the same code can be
  executed with multiple strategies. Compare the [monad transformer
  example](https://github.com/blancas/morph/wiki/Composing-Monads) in
  morph to [those using this
  library](https://github.com/bwo/monads/wiki/An-expression-evaluator):
  morph requires that a special return function be defined for the
  transformer in question, a special lift function, etc., because it
  cannot allow a single generic "return", "lift", etc. The result is
  difficult to change, because assumed evaluation strategy is
  pervasive.

  This is the problem that symbol macros solve in algo.monads: the
  supposedly bare `return` in something like `(return 3)` in fact ends
  up getting the proper return method, through complicated macrology,
  passed in.

- Monad transformers are unnecessarily confusing.

  This may admittedly be a personal problem, but monad transformers
  for both algo.monads and morph strike me as harder and more
  complicated to implement than they need to be. It isn't clear, to
  me, how to to list monads up the stack with algo.monads, which is
  why the `sequence-t` monad transformer needs the `which-m-plus`
  parameter, which shouldn't be necessary at all:

  ```clojure
  monads.list> (run-monad (list-t monads.maybe/m) (mplus (return 5) (return 3)))
  #<Just (5 3)>
  monads.list> (run-monad (list-t monads.maybe/m) (lift (mplus (return 5) (return 3))))
  #<Just (5)>
  ```
  
  In both algo.monads and morph creating a monad transformer stack
  more than two monads deep seems like a touchy proposition.

## License

Copyright © 2013 Ben Wolfson 

Distributed under the Eclipse Public License, the same as Clojure.
