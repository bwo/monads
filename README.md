# monads

Yet another clojure library for monads.

The goals for this library are expressivity and correctness. Things
should do what they say on the tin, and it should not be a pain to
construct complex monadic computations or use stacks of multiple
transformers. Some attention has been paid to performance (for
instance, while initially "base" monads such as `state-m` were defined
using the `state-t` transformer and the `identity-m` monad, now the
base monads all have their own implementation), and in fact this
package appears to be faster than `algo.monads`, but that is not the
primary aim.

The idiom is unabashedly Haskell-derived: the bind function, for
instance, is spelled `>>=`, and the special syntax, `mdo`, apes
Haskell's do-notation far more closely than does `algo.monads`'
`domonad`. (A
[justification/rant](https://bitbucket.org/kenko/macroparser/src/eb372fec0e3a30daf7e8d946cacea4ceba86a0ea/src/macroparser/monads.clj?at=default#cl-7)
on this topic is available.) Compare the solution to tree-labelling
exercise from [the Haskell StateT
documentation](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-State-Lazy.html#g:8)
to the solution in this library:


```clojure
;; Our trees: {:val int :left tree :right tree}, or nil
(defn node [v left right]
  {:val v :left left :right right})
(defn n-node [x table]
  (if-let [i (get table x)]
    [table i]
    (let [c (count table)]
      [(assoc table x c) c])))
(defn number-node [x]
  (mdo table <- get-state
       let [newtable newpos] = (n-node x table)
       (put-state newtable)
       (return newpos)))
(defn number-tree [{:keys [val left right] :as tree}]
  (if-not tree
    (return nil)
    (mdo num <- (number-node val)
         nt1 <- (number-tree left)
         nt2 <- (number-tree right)
         (return (node num nt1 nt2)))))
(defn num-tree [t]
  (eval-state (number-tree t) {}))
```

The equivalent of the monadic `number-node` and `number-tree`
functions using `algo.monad` syntax would be:

```clojure
(defmonadfn number-node [x]
  (domonad [table (fetch-state)
            :let [[newtable newpos] (n-node x table)]
            _ (set-state newtable)]
           newpos))

(defmonadfn number-tree [{:keys [val left right] :as tree}]
  (if-not tree
    (m-result nil)
    (domonad [num (number-node val)
              nt1 (number-tree left)
              nt2 (number-tree right)]
             (node num nt1 nt2))))
```

Implementations are provided for reader, list, maybe, identity,
continuation, state, and error monads, and with transformers for all
except list (since the straightforward list-t implementation does not
always yield a true monad, though I may yet implement
[logic-t](http://hackage.haskell.org/package/logict)) and identity.

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

All monads support the basic `>>=` and `return` operations; all
transformers additionally support `lift`. Additional operations are
supported by only some monads (suffixless names are given in this
list; the base monads are formed by suffixing `-m`, the transformers
by suffixing `-t`):

- `mfail`: supported by `maybe`, `error`, and `writer`, and any
   the result of any transformer whose argument supports it.
- `mzero`, `mplus` (the `monadplus` typeclass): supported by `maybe`,
   `error`, and `list`, and the result of any transformer whose
   argument supports them. 
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

Further such functions are easily defined. This, for instance, is the
definition of `guard`:

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
