{{
let title = "Violating memory safety with Haskell's value restriction"
let date = "17 May 2025"
}}

A common issue in impure ML-style languages with polymorphism and mutable references is the possibility of *polymorphic references*.

In a hypothetical impure language that had both these features, but no mitigations against polymorphic references, the following code would be extremely unsafe.

```hs
unsafeCoerce :: a -> b
unsafeCoerce x = 
    let dangerous = ref Nothing
    dangerous := Just x
    case dangerous of
        Nothing -> error "unreachable"
        Just y -> y
```
This code creates a reference `dangerous` with an initial value of `Nothing`, writes `x` to it, and then reads from it again.
But because this language doesn't prevent polymorphic references and `Nothing` has type `forall a. Maybe a`, `dangerous` is actually *generalized* to `forall a. Ref (Maybe a)`.
This means that in the line that writes to it, `dangerous` is instantiated to `Maybe a`, whereas in the line that *reads* from it, it is instantiated to `Maybe b`, although the value stored in it still has type `a`, breaking type safety and consequently memory safety!

Scary, right?

You might think that you could prevent this by just preventing generalization of reference types, but references can be hidden behind closures, so languages need a slightly blunter hammer: the *value restriction*.

The value restriction says that a let binding can only ever be given a polymorphic type if its bound expression is syntactically a "value", i.e. an expression that obviously will not perform any computation. `5` is a value. So is `Nothing`. But `sqrt 42` and (crucially) `ref Nothing` are *not* values and will not be generalized.[^ocaml]

## Haskell's let bindings do not have a value restriction.
So, does this mean that Haskell's type system is deeply unsound and we just never noticed?

No! If we translate the original example to Haskell, we will hit a type error, telling us that `dangerous` was *not* generalized.
```hs
unsafeCoerce :: a -> b
unsafeCoerce x = do
    dangerous <- newIORef Nothing
    writeIORef dangerous (Just x)
    result <- readIORef dangerous
    case result of
        Nothing -> error "unreachable"
        Just y -> y
```
<!-- ghc output -->
<pre><div style="font-family: "><div><span style="font-weight: bold;">MonadGen.hs:82:19: </span><span style="color: #cd3131; font-weight: bold;">error</span><span style="font-weight: bold;">: [</span><span style="font-weight: bold; text-decoration: underline;">GHC-25897</span><span style="font-weight: bold;">]</span><span>
</span></div><div><span></span><span style="font-weight: bold;">    • Couldn't match type ‘a’ with ‘b’</span></div><div><span></span><span style="font-weight: bold;">      Expected: IO b</span></div><div><span style="font-weight: bold;">        Actual: IO a</span></div></div></pre>

<!-- /ghc output -->

But the interesting question here is: *Why* was it not generalized? To answer that, we will have to take a small step back.

## What even is an IO
The important detail in this Haskell code is that `dangerous` was *not* bound in a `let` binding. It was bound in a monadic `do`-binding.

`IO` is famously a monad, so a `do`-binding like this is just syntactic sugar for an application of `(>>=)`, which (specialized to `IO`) has the following type.
```hs
(>>=) :: forall a b. IO a -> (a -> IO b) -> IO b
```

So the value passed to the continuation (corresponding to the variable in the let binding) has whatever type is wrapped *inside* the `IO` type constructor of its first argument.

Now, `newIORef Nothing` can itself have a polymorphic type.
```hs
newIORef Nothing :: forall a. IO (IORef a)
```

However, notice that the polymorphic forall quantifier occurs *outside* the `IO`! This means that the value passed to the continuation of `(newIORef Nothing >>=)` will always have type `IORef _` and therefore be *monomorphic*. We can instantiate the `a` with `forall a. Maybe a`, but that only gives us a perfectly safe `IORef (forall a. Maybe a)`[^nothing], *not* a `forall a. IORef (Maybe a)`.

That's the magic! The placement of the `IO` in the types prevents giving `dangerous` a polymorphic type.

What I really want you to appreciate here is just how similar this is to the traditional value restriction. If we're allowed to do arbitrary effects, we might create a polymorphic reference, so ML bans all let bindings that look like they might perform effects, whereas Haskell already has a distinction between pure and effectful let bindings (`>>=`) and just needs to prevent the second group from being generalized.

## Generalizable Monads
This hopefully all seems reasonable so far. `IO` can create mutable references so we need to prevent it from generalizing them and that's why the monadic interface imposes something resembling the value restriction.

But... `IO` is not the only monad. If we look at [`Identity`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Functor-Identity.html#t:Identity), it's a monad that quite literally does nothing, so a `do`-binding in `Identity` is just a pure `let`-binding. Shouldn't we at least be able to generalize `do`-bindings in a trivial monad like this?

Let's think through what that would mean. Above, we couldn't generalize `newIORef Nothing` because it had type `forall a. IO (IORef (Maybe a))` with the `forall` on the *outside*. If it had type `IO (forall a. IORef (Maybe a))`, we could instantiate the `a` parameter to `(>>=)` with `forall a. IORef (Maybe a)` and therefore generalize it.

Coming back to `Identity`, we can generalize Identity bindings, if for any context `f` (e.g. `f ~ IORef`), we can turn something of type `forall a. Identity (f a)` into something of type `Identity (forall a. f a)`. More generally, we can define a type class for monads where we can generalize bindings this way.

```hs
class (Monad m) => MonadGen m where
    generalize :: forall f. (forall a. m (f a)) -> m (forall a. f a)
```

So, can we define this for Identity? Yes![^binding]
```hs
instance MonadGen Identity where
    generalize m = do
        let (Identity x) = m
        Identity x
```

If we use this new fancy function, our monadic bindings in `Identity` can be just as powerful as regular let bindings! (though a little less convenient since we need newtypes)

```hs
newtype Endo a = Endo (a -> a)

applyEndo :: Endo a -> a -> a
applyEndo (Endo f) x = f x

blah :: Identity (Bool, Char)
blah = do
    -- f :: forall a. Endo a <- pure (Endo (\x -> x))  -- fails
    f :: forall a. Endo a <- generalize (pure (Endo (\x -> x))) -- succeeds
    pure (applyEndo f True, applyEndo f 'a')
```

Nice! Since we made this a type class, you're probably already wondering which other monads we can implement it for.
Turns out, there are actually quite a few! For reasons that will become apparent in a moment, let's look at...

## State[^state]

The `State` monad in Haskell is defined as[^stateDetails]
```hs
newtype State s a = State (s -> (s, a))
```

In order to simulate mutable state, this definition represents computations as functions that take the current state as a parameter and return a new state value. As it turns out, if we just keep plumbing those state values around, nothing stops us from binding the result of this function in a let binding and thereby generalizing the result of the computation!

```hs
instance MonadGen (State s) where
    generalize m = do
        let (State f) = m
        State \s -> do
            let (s', result) = f s
            (s', result)
```

Great! So it seems like many pure monads do support binding generalization.
Then there must be something about the internal structure of `IO` that is somehow special and prevents
us from implementing `MonadGen IO`, right?

Right?

## What is an IO, *really*?

A popular metaphor for impure functions in pure languages is that instead of modifying the world around it, a function like `putStrLn` essentially takes the real world as a parameter and returns a modified version of it where a string has been written to `stdout`.
Interestingly enough, this is very close to how `IO` works internally!

```hs
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

Of course, we cannot literally modify the real world as a pure value, so internally, `putStrLn` is an actual, impure function.

But the `State#` values still act both as capabilities (ensuring that impure functions can only be called from impure functions) and data dependencies (to ensure that impure functions are evaluated in the correct order despite laziness).

Using this constructor directly can be unsafe, since the illusion of purely modifying the real world (and `IO`'s sequencing guarantees) only apply if the `State# RealWorld` tokens are passed around *linearly*, i.e. are never duplicated or dropped. However, if we manually make sure to uphold this invariant and we don't use any further GHC internals, common knowledge suggests that we should be safe.

## That type definition looks familiar

It uses an unboxed tuple instead of a boxed one and is specialized to (zero-sized) unboxed `State# RealWorld` tokens, but otherwise `IO` is really just a state monad! So, if we were able to implement `MonadGen` for `State`, shouldn't we be able to implement it for `IO` as well?

If we tried to copy the `State` definition directly, we would hit a quite awkwardly phrased error message[^awkward]

```hs
instance MonadGen IO where
    generalize m = do
        let (IO f) = m
        IO \s -> do
            let (# s', result #) = f s
            (# s', result #)
```
<pre><div style="font-family: "><div><span></span><span style="font-weight: bold;">MonadGen.hs:51:17: </span><span style="color: #cd3131; font-weight: bold;">error</span><span style="font-weight: bold;">: [</span><span style="font-weight: bold; text-decoration: underline;">GHC-20036</span><span style="font-weight: bold;">]</span></div><div><span></span><span style="font-weight: bold;">    You can't mix polymorphic and unlifted bindings:</span></div><div><span></span><span style="font-weight: bold;">      (# s', result #) = f s</span></div><div><span style="font-weight: bold;">    Suggested fix: Add a type signature.</span></div></div></pre>

Unfortunately, the suggested fix doesn't help us. Giving `result` a polymorphic type is the whole point here!

Fortunately, we can just box the `State# RealWorld` token first and avoid the issue.

```hs
data BoxedState s = BoxedState {state :: State# s}

liftState :: (# State# s, b #) -> (BoxedState s, b)
liftState (# s, b #) = (BoxedState s, b)

instance MonadGen IO where
    generalize m = do
        let (IO f) = m
        IO \s -> do
            let (boxedState, result) = liftState (f s)
            let (BoxedState{state}) = boxedState
            (# state, result #)
```

And this compiles! So, does that mean...

## Yes!

```hs
newtype MaybeRef a = MaybeRef (IORef (Maybe a))

unsafeCoerceIO :: forall a b. a -> IO b
unsafeCoerceIO x = do
    maybeRef <- generalize (MaybeRef <$> newIORef Nothing)
    let MaybeRef ref = maybeRef
    writeIORef ref (Just x)
    readIORef ref >>= \case
        Just y -> pure y
        Nothing -> error "unreachable"
```
<span></span>
```hs
λ> unsafeCoerceIO @_ @String id
"fish: Job 1, 'ghci MonadGen.hs' terminated by signal SIGSEGV (Address boundary error)
```


## Conclusion
What I want you to take away from this is that

- Despite its purity, Haskell *does* need something resembling the value restriction, just like every other ML with references.
- However, this value restriction is only given by the monadic interface of `IO` and *not* inherent to its definition.
- Contrary to popular belief, unwrapping the `IO` constructor is deeply unsafe and can violate memory safety, even if `State#` tokens are never duplicated or dropped.



[^ocaml]: This is quite a strong restriction, so languages like OCaml use [a slightly more flexible approach](https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf). The core idea of restricting generalization of function calls that don't clearly return values that are safe to generalize remains though.
[^nothing]: It's just a reference that can only store values of the form `Nothing`, `Just undefined` or `undefined`.
[^binding]: Even though it looks a little strange, the let binding to bind `Identity x` is necessary! Directly matching on `m` wouldn't allow it to be generalized. This also relies on `MonoLocalBinds` (implied by `TypeFamilies`, `GADTs` and `LinearTypes`) being off for this module, since that would prevent generalization of the let binding that mentions a variable from an outer scope.
[^state]: If you know a little about GHC internals, you might see where I'm going with this.
[^stateDetails]: The [actual definition](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#t:State) in `transformers` is a type synonym for `StateT Identity`. It's possible to define a `MonadGen` instance for `StateT m` (provided `m` implements `MonadGen`), but that only detracts from the main point, so we will use the non-transformer `State`. I've also changed the argument order to make it a little more sensible.
[^awkward]: That's how you know you're using the fun parts of GHC