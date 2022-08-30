<!--title: Coherent Local Instances with Dynamic Types and <code>ImplicitParams</code>-->
<!--date: 29 August 2022-->
<!--pubDate:2022-8-29-->
<!--reddit:https://www.reddit.com/r/haskell/comments/x1jz4w/coherent_local_instances_with_dynamic_types_and/-->
If you read my [previous post](/posts/unsafeCoerceDict.html)[^1] about local instances, you might have noticed something.
Remember how I said that `ImplicitParams` are really just [syntactic sugar for instances of the type class `GHC.Classes.IP`](/posts/unsafeCoerceDict.html#implicitparams)? Well, unlike `withFakeDict`, `ImplicitParams` can safely be overridden, without causing incoherence issues.
Couldn't we somehow use `ImplicitParams` to provide local type class instances that can safely be overridden?

Most type classes are a bit more complex than they look, so let's invent a new one to use as a running example.
```hs
class Pretty a where
    pretty :: a -> String
```
`Pretty` is just like `Show`, but with fewer methods and no preexisting instances that might get in our way.

We also need to define a dictionary type for `Pretty`. We could use the Template Haskell machinery from last time for this,
but `Pretty` is simple enough to do it manually for now.

```hs
-- The newtype is important since `Pretty` only has a single method
newtype PrettyDict a = PrettyDict {
    _pretty :: a -> String
}

boringPretty :: PrettyDict Int
boringPretty = PrettyDict {
    _pretty = show
}

nicePretty :: PrettyDict Int
nicePretty = PrettyDict {
    _pretty = \x -> "✨" <> show x <> "✨"
}
```

With this, we could already write a few functions that use our 'type class', just by accepting a corresponding dictionary as an implicit parameter. This way we actually get something closely resembling local instances already!

```hs
pretty :: (?prettyInst :: PrettyDict a) => a -> String
pretty = _pretty ?prettyInst

f :: (?prettyInst :: PrettyDict a) => [a] -> String
f xs = intercalate ", " (map pretty xs)

main :: IO ()
main = do
    let ?prettyInst = boringPretty
    putStrLn $ f [1, 2, 3 :: Int]
    let ?prettyInst = nicePretty in putStrLn $ f [1, 2, 3 :: Int]
```

Let's also enable optimizations, just to make sure this is actually coherent. :)

```txt
$ ghc -O2 Main.hs && ./Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
5
✨5✨
```

Perfect! 

There is just one issue. While `ImplicitParams` make it almost trivial to work with *local* instances,
we lose the ability to define regular *global* instances.

Whenever we want to use a function with a `?prettyInst` constraint, we always have to define the instance we want to use somewhere locally in its lexical scope.

If we wanted an actual replacement for type classes with local *and* global instances, we would need some kind of...

## Global `ImplicitParams`?

Remember how `IP` is just a regular type class? Couldn't we just... you know... define a regular, global instance for `IP`?

```hs
instance IP "x" Int where
    ip = 5

λ> ?x
5
```

So... uh... I honestly did not expect this to work, considering GHC forbids manual `Typeable` or `Generic` instances.

Anyway, if we want to model our class with this, we can simply write global instances as instances for `IP` and override local instances with `let ?x = ...` just as we did before.

```hs
instance IP "prettyInst" (PrettyDict Int) where
    pretty = boringPretty

main :: IO ()
main = do
    putStrLn (pretty (5 :: Int))
    putStrLn (let ?prettyInst = nicePretty in pretty (5 :: Int))
```

```bash
$ ghc -O2 Main.hs && ./Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
5
✨5✨
```
Awesome! Unlike our unsafeCoerce trick last time, optimizations don't break `ImplicitParams`, so this is entirely safe!

We just came up with a type class replacement that completely subsumes local, as well as global instances in a few lines of code.

It's that simple!

## It's not that simple
If you look at the definition of `IP`, you will likely notice the deadly flaw in our current approach

```hs
class IP x a | x -> a where
    ip :: a
```
**There is a functional dependency between the parameter name and its type.**[^2]
This means that every parameter name can only ever be used with *the same type argument* and so our type class replacement can only ever have a single instance per class. 
The entire purpose of type classes is to overload operations for different types, so this completely defeats the point of using type classes in the first place. :/

If we try to write an instance for two separate types anyway, the compiler is going to complain as expected, since we violated the functional dependency.

```hs
instance IP "prettyInst" (PrettyDict Int) where
    pretty = boringPretty

-- Error: Functional dependency conflict
instance IP "prettyInst" (PrettyDict String) where
    pretty = PrettyDict id
```

## <a href="https://www.youtube.com/watch?v=g8ufRnf2Exc" target="_blank">When life gives you lemons</a>
Unfortunately, since we can only have a single global `IP` instance, we cannot use Haskell's regular type class dispatching mechanism to select the instance that we want.

But... 

we could try and build our own, *based on runtime type information*.
This solution will not work for all types, but it might work for all that implement `Typeable`.

With `Typeable`, we can extract a `TypeRep`[^3] from a `Proxy` for a given type and, thanks to `typeRepFingerprint`, we are able to turn this `TypeRep` into a `Fingerprint` that is unique for every type. This `Fingerprint` implements `Ord`, so we can use it as a key in a `Map`.

With this, we can build some machinery that selects an instance from a `Map` based on the runtime `TypeRep` of a type.
The values of the `Map` then represent the instances for the given types. 

Conveniently, we already have a way of reifying instances, so we can just store the concrete dictionaries (`PrettyDict`s in this case).

We have to store dictionaries of different types (e.g. `PrettyDict Int`, `PrettyDict Bool`, ...), so we could try some tricks with existentials, but since we are relying on runtime type information anyway, it is easier to store them as [`Any`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Exts.html#t:Any) and use `unsafeCoerce`.

```hs
newtype InstMap = MkInstMap {unInstMap :: Map Fingerprint Any}
```

To actually use this custom dispatching mechanism, we need to reimplement our `pretty` 'method', which now takes an `InstMap` as an implicit parameter and extracts the dictionary for the type used, based on its `TypeRep`.

```hs
pretty :: forall a. (Typeable a, ?prettyInst :: InstMap) => a -> String
pretty =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    case M.lookup key (unInstMap ?prettyInst) of
        Nothing -> error "pretty: No instance found at runtime"
        Just dict -> _pretty $ unsafeCoerce dict
```

To add a local instance to the instance map, all we have to do is to update the implicit `?prettyInst` parameter.

```hs
withPretty :: forall a b. Typeable a => PrettyDict a -> ((?prettyInst :: InstMap) => b) -> b
withPretty dict x =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    let MkInstMap prevInst = ?prettyInst in
    let ?prettyInst = MkInstMap (M.insert key (unsafeCoerce dict) prevInst) in 
    x
```

Okay, so we gave up type safety, but in return, we gained the ability to override instances locally... which we could already do with just `ImplicitParams` without any runtime type dispatch machinery. Sounds like we are back to square one?

## Recovering type safety
We have a way of locally overriding instances, but we should really make sure that instances are actually available.

How could we check this? Well, type classes are perfect for invariants like this!

If using type classes to implement a type class replacement sounds pointless, keep in mind that we don't care about the type class methods. In fact, we don't even need any! This way, incoherence is not an issue, since the concrete instance chosen is irrelevant.

```hs
class HasPretty a

pretty :: forall a. (Typeable a, HasPretty a, ?prettyInst :: InstMap) => a -> String
pretty = -- same implementation
```

Note that we didn't change anything on the term level. The `HasPretty` constraint in the type purely exists to make sure that we call `withPretty` at some point before calling `pretty`.

To satisfy the `HasPretty` constraint in the continuation passed to `withPretty`, we can just use the `unsafeCoerce` trick from the previous post.

```hs
data LocalPrettyInst

instance HasPretty LocalPrettyInst

withPretty :: forall a b. Typeable a 
           => PrettyDict a 
           -> ((HasPretty a, ?prettyInst :: InstMap) => b) 
           -> b
withPretty dict x =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    let MkInstMap previousInst = ?prettyInst in
    case unsafeCoerce (Dict :: Dict (HasPretty LocalPrettyInst)) :: Dict (HasPretty a) of
        Dict -> 
            let ?prettyInst = MkInstMap (M.insert key (unsafeCoerce dict) previousInst) in 
            x
```
So, a function that would have a `Pretty a` constraint with regular classes or a `(?prettyInst :: PrettyDict a)` constraint with `ImplicitParams`, now needs a `(Typeable a, HasPretty a, ?prettyInst :: InstMap)` constraint.

## Going global

We recovered type safety, but our approach still doesn't offer any advantage over pure `ImplicitParams`, since we still don't have a way to write global instances.

Your first thought might be to write a global instance for `IP "prettyInst" InstMap` and somehow populate that when defining new global instances.

Unfortunately, this doesn't work. We could try to use a global `IORef` and extend it at runtime, but how would we run the code to extend that IORef? Haskell doesn't provide a way to run IO code at module load time, so we couldn't make sure that the map contains all relevant instances without having to populate it in `main`.

Instead, let's start with an empty map.

```hs
instance IP "prettyInst" InstMap where
    ip = MkInstMap mempty
```

The typechecker prevents us from using this instance without further local instances since we don't have any `HasPretty` instances yet.

Fortunately, telling the type checker about global instances is quite easy: We just have to implement `HasPretty` for the type we are writing an instance for.

Since we don't have an actual instance yet, this would still crash at runtime; our instance map doesn't have any implementations by default.

Consider this: In case the instance map does not contain an entry for the type `pretty` is called at, but it *is called* (meaning the `HasPretty` constraint was satisfied), we know that there *has to be a global instance* and that *no local instances are in scope*. Thus, crucially, **there is only a single instance for `HasPretty`, which is a regular global instance**. If there is only a single instance, we don't need to deal with incoherence and we are able to use actual methods from `HasPretty` with confidence that they are coming from the global instance with or without optimizations.

What would possible methods on `HasPretty` look like? We still need to specify the implementation of our global instance somewhere, so this would be a perfect place to put that.

Now, whenever `pretty` is called and detects a runtime instance in its instance map, we know that the instance has been overridden and `HasPretty` is potentially contaminated, so we use the instance from the instance map directly and ignore any methods from `HasPretty`.

If there is no runtime instance, we know that the instance it is called at has to be a global, coherent instance and we can safely use the implementation from `HasPretty` to maintain coherence.

```hs
class HasPretty a where
    globalInst :: PrettyDict a

data LocalPrettyInst

instance HasPretty LocalPrettyInst where
    -- This will never be called unless someone seriously messes with things, 
    -- since `withPretty` always adds an instance to the ?prettyInst InstMap 
    globalInst = error "pretty: No instance found at runtime"

pretty :: forall a. (Typeable a, HasPretty a, ?prettyInst :: InstMap) => a -> String
pretty =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    case M.lookup key (unInstMap ?prettyInst) of
        Nothing -> 
            _pretty globalInst
        Just dict ->
            _pretty $ unsafeCoerce dict
```

This works!

If we try the example from before...

```hs
instance HasPretty Int where
    globalInst = boringPretty

main :: IO ()
main = do
    putStrLn (pretty (5 :: Int))
    putStrLn (withPretty nicePretty (pretty (5 :: Int)))
```
```bash
$ ghc -O2 Main.hs && ./Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
5
✨5✨
```

... everything works as expected, even with optimizations!

There is actually not much boilerplate involved in this technique, compared to regular type classes and instances.

The main limitation is that types used in instances all have to implement `Typeable`, so we can only use this for relatively simple instances.

Defining a class is quite a bit more verbose since we have to define a dictionary type, a `HasX` class to carry the global instance, a global instance for the associated implicit parameter as well as functions to apply and override the active instance.

To use our running example, the class definition
```hs
class Pretty a where
    pretty :: a -> String
```
becomes
```hs
newtype PrettyDict a = PrettyDict {
    _pretty :: a -> String
}

class HasPretty a where
    globalInst :: PrettyDict a => a

pretty :: forall a. (Typeable a, HasPretty a, ?prettyInst :: InstMap) => a -> String
pretty =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    case M.lookup key (unInstMap ?prettyInst) of
        Nothing -> 
            _pretty globalInst
        Just dict ->
            _pretty $ unsafeCoerce dict

data LocalPrettyInst

instance HasPretty LocalPrettyInst where
    globalInst = error "pretty: No instance found at runtime"

withPretty :: forall a b. Typeable a => PrettyDict a -> ((HasPretty a, ?prettyInst :: InstMap) => b) -> b
withPretty dict x =
    let key = typeRepFingerprint (typeRep (Proxy :: Proxy a)) in
    let MkInstMap prevInst = ?prettyInst in
    case unsafeCoerce (Dict :: Dict (HasPretty LocalPrettyInst)) :: Dict (HasPretty a) of
        Dict -> 
            let ?prettyInst = MkInstMap (M.insert key (unsafeCoerce dict) prevInst) in 
            x
```

Crucially, though, the boilerplate involved in *using* these overridable instances is pretty minimal and most of the boilerplate when defining these 'classes' could be automated with TemplateHaskell.

New global instances only have to implement `HasPretty`, which is a tiny bit less ergonomic than `Pretty`, since
we have to implement the dictionary, not the methods and there are no default implementations.

```hs
instance HasPretty Int where
    globalInst = PrettyDict {
        _pretty = show
    }
```

The only difference between a function that uses our overridable `pretty` and one that uses the regular `Pretty` type class is in the constraint, which changes from `Pretty a` to `(Typeable a, HasPretty a, ?prettyInst :: InstMap)`.

We could try to factor this out to a type synonym.
```hs
type Pretty a = (Typeable a, HasPretty a, ?prettyInst :: InstMap)
```

Thereby completely eliminating any additonal boilerplate when using `pretty`!

Let's rewrite `pretty` with a `Pretty a` constraint instead of the more complicated `(Typeable a, HasPretty a, ?prettyInst :: InstMap)`. After all, `Pretty` is just a type synonym, so this should mean exactly the same thing, right? ...*right?* 

## It all comes tumbling down

```hs
pretty :: Pretty a => a -> String
pretty = ...

λ> :t pretty
pretty :: (HasPretty a, Typeable a) => a -> String
```

Uh oh. The `?prettyInst :: InstMap` constraint just... disappeared?

What happened here, is that when resolving[^4] the type synonym, GHC tried to simplify the constraint and removed the monomorphic `?prettyInst :: InstMap` constraint, since an instance was found in the global context.

This obviously sounds like a bug, but I am not certain that it actually is. In fact, the real bug is probably that we were able to define a global instance for `IP` in the first place.

Okay, we are unable to factor out the `(Typeable a, HasPretty a, ?prettyInst :: InstMap)` constraint with a type synonym. Since we have a custom `HasPretty` class anyway, we could try to include these as superclasses in the style of [Opaque constraint synonyms](https://blog.csongor.co.uk/opaque-constraint-synonyms) instead.

```hs
class (Typeable a, IP "prettyInst" InstMap) => HasPretty a
```

...except this doesn't work. GHC does not allow implicit parameters in superclasses, even if we explicitly write them as `IP` class constraints. 

This is a reasonable restriction because the time at which dictionaries with super classes are constructed can be quite unpredictable, so an implicit parameter superclass constraint will probably not contain the intended value, but it *is* annoying since we have to keep the `?prettyInst :: InstMap` constraint around.

We could at least try to factor out `Typeable` though.

```hs
class Typeable a => HasPretty a where
    ...
```

This compiles and even runs correctly!

...as long as we compile *with* optimizations.[^5]

So, factoring out the `(Typeable a, HasPretty a, ?prettyInst :: InstMap)` constraint is not an option. Is there anything else we cannot do?

```hs
f :: (Typeable a, HasPretty a, ?prettyInst :: InstMap) => a -> String
f = pretty

g x = f x

λ> :t g
g :: (Typeable a, HasPretty a) => a -> String

main :: IO ()
main = do
    -- I am using instances for `Bool` here since we haven't defined a 
    -- global instance for `HasPretty Bool` yet.
    putStrLn (withPretty (PrettyDict @Bool show) (f True))
    putStrLn (withPretty (PrettyDict @Bool show) (g True))

```
```bash
$ ghc Main.hs && ./Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
True
Main: pretty: No instance found at runtime
CallStack (from HasCallStack):
  error, called at Main.hs:55:18 in main:Main
```

Oh, come on!

As it turns out, when inferring the type for `g`, GHC omits the implicit parameter constraint and instead hard wires the empty instance map from the global instance.

`withPretty` still inserts a `HasPretty` instance, so the compiler cannot stop us from calling `g` and crashing at runtime.

## Conclusion

* We are able to replace type classes with overridable `ImplicitParams` and runtime type information based dispatch.
* We can recover type safety, by using fake constraints for an empty class so that incoherence is not an issue.
* To get proper global instances, we can extend the previously empty class with a base implementation. We only ever use this implementation if we are absolutely sure no local instance exists, so this is also safe.
* Unfortunately, the global `ImplicitParams` instance makes `?prettyInst` constraints extremely brittle and mistakes like forgetting a type signature can remove the parameter and lead to runtime crashes.

To safely use this, we *always* have to make sure to

* type out the entire `(Typeable a, HasPretty a, ?prettyInst :: InstMap)` constraint
* write a type signature whenever it might include `?prettyInst`

Now, should you ever use any of this in anything that you might possibly not want to break?

Of course not. Global implicit parameters seem to be mostly uncharted territory and I'm pretty sure their mere existence constitutes a bug.

Still, screwing around with broken features and pushing the limits of Haskell is fun. I had a blast writing this and I hope it was enjoyable to read as well! Maybe you even learned something today?

[^1]: If you have not already, you should probably read [that post](/posts/unsafeCoerceDict.html) first to understand the context of this and what 'incoherence' even means.

[^2]: As far as I can tell, the reason for this is to aid type inference. Without this dependency, GHC would be unable to figure out the type of `?x` even when a `(?x :: Int)` constraint is in scope. This is a little frustrating though, since the functional dependency is a bit of a lie. The type of `x` is not *really* only determined by its name. It is determined by its name and the instance in the surrounding context, but GHC cannot express that so it cheats and invents a functional dependency.

[^3]: The terminology is a bit confusing here. `TypeRep` in `Data.Typeable` is just a representation of regular compile-time types, that is carried in `Typeable` constraints at runtime, not about the runtime representation of values as in `type role representational`. Concretely this means, that newtypes like `Int` and `Sum Int` will have the same runtime representation, but their `TypeRep`s will be different.

[^4]: The issue might have already happened when *defining* the type synonym. I'm not sure but the end result is the same either way.

[^5]: Yeah...
