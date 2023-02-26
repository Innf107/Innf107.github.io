<!--title: Fast Map Union and Local Instances Through Instance Types -->
<!--date: 26 February 2023-->
<!--pubDate:2023-02-26-->
<!--reddit:https://www.reddit.com/r/haskell/comments/11c27fh/fast_map_union_and_local_instances_through/-->

In part 3[^previous] of my [crusade against](/posts/unsafeCoerceDict.html) [GHC's coherence guarantees](/posts/coherentIP.html), I have actually done it! This time we will end up with a way to generate local type class instances without any asterisks about code breaking with optimizations. On the way, we are going to end up solving the dreaded Fast Map Union Problem, combining two of my favorite Haskell tricks, and discovering a bug in a previous version of GHC.

But before we get to that, let's start at the beginning

## What even is this 'fast Map union problem'?

Let's pretend that Haskell *did* have consistent locally overridable type class instances. In that case, the interface for `Map` would be completely broken.

Look at the type of `Data.Map.insert`

```hs
insert :: Ord k => k -> v -> Map k v -> Map k v
```

See the issue? `Map` is some kind of ordered tree internally, so it depends on the `Ord` instance being consistent across different operations, but with local instances, we don't have any guarantees like that.

```hs
-- This uses the regular instance for Ord Int
let map = insert (1 :: Int) 1 mempty

-- This uses a different, incompatible instance on the same map!
withLocalOrd reverseOrdInt $ insert (2 :: Int) 2 map 
```

If you think about this for a bit, you may come up with a solution: You can store the instance in the map. This is how most map implementations in other languages work after all.

```hs
data Map k v = MkMap (Dict (Ord k)) (ActualMapImplementation k v)
```

And now the type of insert doesn't mention `Ord` at all anymore so everything is sunshine and roses

```hs
insert :: k -> v -> Map k v -> Map k v
```

...until it isn't! Consider the type of `union` now

```hs
union :: Map k v -> Map k v -> Map k v
```

What happens if both maps use different `Ord k` instances? We have no way to statically ensure that they don't. The way to solve this in practice would be to just reinsert every value from one map in the other, but that means that union is now drastically slower than it would be if we could assume that both maps use the same instance (It's now at least linear in the size of one map vs. logarithmic in that of the smaller one).

## Instance Types

Now, what we really need here is a way to statically ensure that both maps use the same `Ord` implementation. In a dependently typed language, one could just carry the instance dictionary in the type

```hs
data Map k v (inst :: Ord k) = ...

union :: Map k v inst -> Map k v inst -> Map k v inst
```

But we cannot do that in Haskell (yet?), so we need a few more tricks. What we absolutely *need* to do is to somehow move the used instance to the type level, so let's start there. While we cannot directly depend on the instance value, we can define a dummy type as a stand-in.

With this new type, we can write a version of `Ord` that carries its concrete instance in the type. Every function with an `Ord` constraint should now be polymorphic over this instance parameter.

```hs
class OrdI inst a | inst -> a where
    compareI :: a -> a -> Ordering

data RegularOrdInt
instance OrdI RegularOrdInt Int where
    compareI = compare

data ReverseOrdInt
instance OrdI ReverseOrdInt Int where
    compareI x y = case compare x y of
        LT -> GT
        EQ -> EQ
        GT -> LT
```

And now we can finally parameterize Map over the used `Ord` instance!

```hs
data IMap inst k v = ...

insert :: OrdI inst k => k -> v -> IMap inst k v -> IMap inst k v
union :: OrdI inst k => IMap inst k v -> IMap inst k v -> IMap inst k v
```

`union` statically ensures that both maps agree on their instances.

The only 'issue' here is that we lose some inference. This is more or less unavoidable if we want multiple instances to coexist (well, more on that later). We need to tell Haskell what instance to use at some point, but it's not that bad, since at least for `IMap`s, which carry the instance in their type, we only need to do this once.

```hs
let map1 = insert 1 1 $ insert 2 2 $ empty @RegularOrdInt
let map2 = insert @ReverseOrdInt 3 3 $ insert 4 4 $ empty

union map1 map2 -- Type error: Couldn't match type 'RegularOrdInt' with `ReverseOrdInt`
```

## How do local instances fit into this?

Quite well as it turns out! We can combine two of my favorite Haskell tricks to implement them in current Haskell (You can stop pretending that they exist. We are going to implement them for real now)

Local instances might differ between different executions of the same code, so we need to make sure that the instance types can only be used locally and cannot escape. If this sounds familiar that is because there is a decent chance that you have used something similar before: [`runST`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html#v:runST).

`ST` is a monad that is used for local mutability inside pure code. You can use it to create `STRef`s and mutate them, just like you would in your favorite imperative language[^ocaml], except that Haskell, by virtue of being a pure language, needs to make absolutely sure that you never ever return an `STRef` from `runST`. Doing so would allow effects in one usage of the supposedly pure `runST` to affect the result of another one.

How does `ST` do this? It uses higher-rank types!

```hs
newSTRef :: a -> ST s (STRef s a)
runST :: (forall s. ST s a) -> a
```

If you have never seen this before, you're probably quite confused right now. The type `forall s. ST s a` specifies that the argument to `runST` needs to be an `ST` value that works for any possible instantiation of `s`. Crucially for this case, this means that the `s` variable is, unlike `a`, *not* a type parameter of `runST` itself, but actually one of *the arguments to runST*. This is much clearer if we write `runST` with an explicit outer `forall`.

```hs
runST :: forall a. (forall s. ST s a) -> a
```

Now, the reason this ensures that no STRefs escape is that the `s` parameter of the `STRef` is the same as that of the containing `ST` monad. If you *were* able to return an `STRef` from the argument to `runST`, e.g. by passing something of type `forall s. ST s (STRef s Int)`, what type would the result have? Blindly substituting would yield `STRef s Int`, but what is `s` now? Previously, `s` was bound by the `forall` in the type of the argument to `runST`, but that `forall` does not exist anymore! This is why GHC will not accept this code and complain about an 'escaping skolem'.

We can use exactly the same trick for local type class instances to invent an instance type that only exists locally!
If we can implement a function of the following type, then this will ensure that instance types cannot possibly escape.

```hs
withOrdI :: forall a b. (a -> a -> Ordering) -> (forall inst. OrdI inst a => b) -> b
```

Now, how do we implement this function? If you read [the first post in this series](/posts/unsafeCoerceDict.html), you will probably know the answer already.

At runtime, GHC represents type classes via a technique called dictionary passing. This means that a function with an `Ord` constraint like `Ord a => a -> a -> Ordering` will be turned into a function that *takes an implementation of that type class as an argument* (`Ord a -> a -> a`). This implementation, called a dictionary, is just a regular record-like data type that contains an implementation for every method. 

Using `GADT`s, we can capture this dictionary in a value.

```hs
data OrdIDict a where
    OrdIDict :: Ord a => OrdIDict a
```

If we are now able to replicate the structure of this `OrdIDict` exactly, but with a custom record of methods, we can use `unsafeCoerce` to convert it to a functional `OrdIDict a`. By pattern matching on the result, we can release the dictionary back into a regular instance, which now contains our handwritten type class instance!

Instinctively, your definition of `OrdIDict` would probably look like this
```hs
data OrdIDict inst a where
    OrdIDict :: OrdI inst a => OrdIDict inst a
``` 

This is not going to work though. We need to invent a new type for `inst`, so `OrdIDict` cannot take it as a parameter. Thanks to Haskell's support for existential types, this is not actually an issue, since we can just leave it off[^skolem].

```hs
data OrdIDict a where
    OrdIDict :: OrdI inst a => OrdIDict a
```

`OrdI` only has a single method, so it will be represented by the equivalent of a newtype record at runtime. This is erased entirely, so we do not need to build a record around our `a -> a -> Ordering` function.

The `OrdIDict` wrapper adds some indirection though, so we need to replicate that.
```hs
data FakeDict a = FakeDict a
```

All preparations are complete. We are ready for the magic[^magic]!

```hs
withOrdI :: forall a b. (a -> a -> Ordering) -> (forall inst. OrdI inst a => b) -> b
withOrdI dict body = 
    case unsafeCoerce (FakeDict dict) :: OrdIDict a of
        (OrdIDict @inst) -> body @inst
```

And that's... it!

Well, not quite. If you paid very close attention, you may notice that we have no way to bind the `inst` type variable in the `(forall inst. OrdI inst a => b)` argument. This means if we put a lambda there, we cannot actually mention `inst` anywhere. This is easy enough to solve by introducing a [`Proxy`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Proxy.html#t:Proxy) value.

```hs
withOrdIProxy :: forall a b. (a -> a -> Ordering) -> (forall inst. OrdI inst a => Proxy inst -> b) -> b
withOrdIProxy dict body = 
    case unsafeCoerce (FakeDict dict) :: OrdIDict a of
        (OrdIDict @inst) -> body @inst (Proxy @inst)
```

## Let's try it out!

We can start with some concrete instances

```hs
map1 = insert 1 1 (empty @Int)
map2 = insert 2 2 (empty @Int)

map3 = insert 3 3 (empty @ReverseIntOrd)
map4 = insert 4 4 (empty @ReverseIntOrd)

fine1 = union map1 map2
fine2 = union map3 map4

notFine = union map1 map3 -- fails!
```

Looking good. Now let's try local instances

```hs
local = withOrdIProxy compare \(Proxy @inst) -> do
    let localMap1 :: IMap inst Int Int = insert @inst 1 1 (empty @inst)
    ()
```
```bash
$ ghc-9.2 insttypes.hs
insttypes.hs:80:42: error:
    • Could not deduce (OrdI (*) Int) arising from a use of ‘insert’
      from the context: OrdI inst a0
    ...
```
Huh. That is... strange. It looks like for some strange reason, GHC defaults the `inst` parameter to... `(*)`[^star]? Even if we add as many type annotations as physically possible?

 If this smells like a bug in GHC, that is **because it is!** Or well, *was*. 

If you run the same example with GHC 9.4, it will compile without complaining.

Now, let's try that again

```hs
local = withOrdIProxy compare \(Proxy @inst) -> do
    let localMap1 = insert 1 1 (empty @inst)
    let localMap2 = insert 2 2 (empty @inst)

    let perfectlyFine = union localMap1 localMap2

    let notFine :: IMap RegularIntOrd Int Int = insert @inst 2 2 (empty @inst) -- fails!
    let notFine2 = union localMap1 map1 -- also fails!

    localMap1 -- fails!
```

Perfect!

### Do we have to annotate the instance every time?

In this specific example with `IMap`, the number of type annotations required was quite manageable, but in code that uses `OrdI` like regular `Ord`, this is much less pleasant

```hs
min3 :: IOrd inst a => a -> a -> a -> a
min3 x y z = case compare @inst x y of -- needs a type application!
    GT -> case compare @inst y z of -- this one as well
        GT -> z
        _ -> y
    _ -> case compare @inst x z of -- same here
        GT -> case compare @inst y z of -- also here
            GT -> z
            _ -> y
        _ -> x
```

Used directly like this, this technique is really just [Scrap your type classes](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html) with extra steps.

This is frustrating because we are just running up against GHC's stubbornness here. There is only a single possible instance for `IOrd inst a` in scope, but GHC doesn't want to choose it. Usually, this behavior might make sense, but it's really not helpful for us.

Fortunately for us though, we are not the first to run into this issue! [Polysemy](https://hackage.haskell.org/package/polysemy), the popular effect system library, had the exact same problem. Polysemy makes it possible to define effects of the form `Member Effect r` over a set of effects `r`. The issue here is that, unlike [mtl](https://hackage.haskell.org/package/mtl), Polysemy allows duplicate effects, so GHC again doesn't trust that you really wanted to use the constraint from the signature and not another, currently unwritten one.

How did Polysemy solve that? They wrote [a type checker plugin](https://hackage.haskell.org/package/polysemy-plugin) that disambiguates Polysemy constraints whenever there is exactly one relevant instance in scope.

This is exactly what we want, so we could probably copy most of it. 

But that is a topic for a future blog post.

## Conclusion

We covered quite a bit of ground there.

- We parameterized type classes over a type representing the instance
- This made it possible to statically ensure that maps can safely be merged in logarithmic time, even in the presence of local instances
- We managed to implement local type class instances that are actually safe, even in the presence of optimizations
- We discovered a bug in GHC 9.2
- There is probably a way to avoid unnecessary type applications with a GHC plugin

This is the first time in this series that I can write a conclusion without begging you to 'please never ever use this anywhere near production'. This technique still relies on GHC's internal type class representation (and you should always be careful around `unsafeCoerce`) but so does the popular [reflection](https://hackage.haskell.org/package/reflection) package, so you should be fine.

If you want to try this for yourself, you can get the code [in a gist](https://gist.github.com/Innf107/53f1b6e2fdbb1ca0a49d1f75375c036c). Just make sure to use GHC 9.4 or above.


[^previous]: You don't need to have read any of the previous posts to understand this, don't worry. On the other hand, if you like this one, you will probably enjoy the others as well ;)
[^ocaml]:If your favorite imperative language is OCaml or Standard ML that is.
[^skolem]: In fact, pattern matching on a value containing an existentially quantified type variable will turn that variable into a 'skolem', just like the higher rank type in the `runST` example, so we are truly conjuring a new type from thin air with this.
[^magic]: This is an OCaml joke. Don't worry about it.
[^star]: `(*)` is another way of writing [`Type`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Kind.html#t:Type), the type of types. The only reason this is even valid is that modern GHC Haskell has dependent kinds and `Type : Type`.