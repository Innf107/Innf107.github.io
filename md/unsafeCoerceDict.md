<!--title: Faking Local Instances with <code>unsafeCoerce Dict</code>-->
<!--date: 18 March 2022-->
<!--pubDate:2022-3-18-->
<!--reddit:https://www.reddit.com/Ihaventactuallyupdatedthelinkyetsorry-->
When you first learned about Haskell's `Monoid` typeclass, you were probably quite surprised to find out that there is no instance for `Monoid Int` in `base`.

After all, `(+)` is an associative binary operation, and `0` acts as a unit element.
```hs
instance Semigroup Int where
    (<>) = (+)
instance Monoid Int where
    mempty = 0
```

The reason, as you will have learned is that this is not the *only* possible instance for `Monoid Int`.

The following instance is, in fact, just as valid.
```hs
instance Semigroup Int where
    (<>) = (*)
instance Monoid Int where
    mempty = 1
```

You might also be aware, that the way to work around this is to declare newtype wrappers and to wrap and unwrap
all `Int`s every time the Monoid instance is needed.

```hs
newtype Sum a = Sum {getSum :: a}
newtype Product a = Product {getProduct :: a}

instance (Num a) => Semigroup (Sum a) where
    (Sum x) <> (Sum y) = Sum (x + y)
instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0

instance (Num a) => Semigroup (Product a) where
    (Product x) <> (Product y) = Product (x * y)
instance (Num a) => Monoid (Product a) where
    mempty = Product 1

λ> getSum $ foldMap Sum [1..10] <> Sum 5
60
```
There is a bit of boilerplate involved in defining the newtypes and corresponding instances, but what is hard to swallow
is that quite a bit of boilerplate has to be included at *every single use site*.[^1]

If Haskell had local instances, this would not be an issue since we would not be forced to commit to a single instance and could just tell the compiler the instance we want
without having to constantly wrap and unwrap everything.[^2]

## Introducing `Dict`
GHC uses a technique called *dictionary passing* to compile typeclass dictionaries. This means that in Core (GHC's intermediate language), `=>` is turned into `->`. 
In other words, *constraints become arguments*, called *dictionaries*.

Why is that useful to know? Well, while there is no way to directly access or construct these dictionaries, we *can* reify them with `Dict`.

```hs
data Dict (c :: Constraint) where
    Dict :: c => Dict c
```

Because the constraint `c` appears in the type signature of the `Dict` constructor, the constructor stores the corresponding dictionary, and whenever it is scrutinized in a pattern match, the constraint is made available.

Unfortunately, even with `Dict`, there is no way to construct a dictionary without defining an instance first.

The only way to construct a 'local' instance then is to define an instance for a newtype wrapper, but if we create a Dict from 
that instance, we cannot use the newtype `Dict` for our original type, because `Dict (Monoid Int)` and `Dict (Monoid (Sum Int))` are entirely separate types to the typechecker.

We hit quite the wall there. We know we could use the Dict for our newtype, and we know it would be safe[^3] because newtypes are erased at runtime, but the typechecker
doesn't allow us to use a newtype Dict to get an instance for the original type.

If there was just some way to bypass the typechecker...

## I solemnly swear I am up to no good
Luckily for us: there is!

`unsafeCoerce :: forall a b. a -> b` lets us bypass the typechecker by treating a value of *any* (lifted) type as a value of *any other* (lifted) type.

This is probably one of the most dangerous, if not *the* most dangerous function in GHC's arsenal. 
If you're not careful, you can easily end up with segmentation faults.

We know what we're doing though, so let's try it out!
```hs
monoidSumDict :: Dict (Monoid (Sum Int))
monoidSumDict = Dict

monoidIntDict :: Dict (Monoid Int)
monoidIntDict = unsafeCoerce monoidSumDict

-- If we didn't include the type signature, GHC would be confused about the type of number we're using
λ> case monoidIntDict of Dict -> fold [1..10] <> (5 :: Int)
60
```
Perfect! We just created an instance of `Monoid Int`, that only exists if we pattern match on `monoidIntDict`. This sounds a lot like local instances to me.


## Let's simplify this
What we have so far is already pretty cool, but having to write out a pattern match every time 
is a bit inconvenient.

Fortunately, we can easily factor out the pattern match by turning it into a function.
```hs
withDict :: Dict c -> (c => a) -> a
withDict d x = case d of Dict -> x

λ> withDict monoidIntDict $ fold [1..10] <> (5 :: Int)
60
```
That `(c => a)` argument might look a bit strange, but this is exactly what we're trying to do: 
Take a value or function with a constraint and remove that constraint by inserting the instance stored in our `Dict`.

We can do even better. The only way to construct one of our 'local' Dicts is to create a `Dict` for a newtype and 
cast that to a `Dict` for the original type using `unsafeCoerce`, but users of our library really shouldn't have to write their own `Dict` definition, let alone use `unsafeCoerce`.

```hs
withLocal :: forall c1 c2 d. c1 => (c2 => d) -> d
withLocal x = case c2Dict of Dict -> x
    where
        c1Dict :: Dict c1
        c1Dict = Dict
        c2Dict :: Dict c2
        c2Dict = unsafeCoerce c1Dict 

λ> withLocal @(Monoid (Sum Int)) @(Monoid Int) $ fold [1..10] <> (5 :: Int)
60
```
Now, all that users have to do is supply the right type applications and `withLocal` is going to do everything else for them. Nice!

## Hey Google, what's a segfault?
```hs
λ> withLocal @(Eq Bool) @(Num Int) $ (-1 :: Int) + 2
fish: Job 1, 'ghci unsafeCoerceDict.hs' terminated by signal SIGSEGV (Address boundary error)
```
Oh no[^4]...

We just used `unsafeCoerce` to cast a `Dict (Eq Bool)` to a `Dict (Num Int)`. We really shouldn't be able to do this.

The issue here is that `withLocal` places no constraints on the --- well --- *constraints* (`c1` and `c2`).
In reality, we need both constraints to contain the same typeclass and to only differ in the *argument* to that class.

Let's do that.
```hs
withLocal :: forall c a b d. (c a) => (c b => d) -> d
withLocal x = case cbDict of Dict -> x
    where
        caDict :: Dict (c a)
        caDict = Dict
        cbDict :: Dict (c b)
        cbDict = unsafeCoerce caDict 
```
Note that we did not change anything about the implementation. We just constrained its type signature.

Unfortunately, `withLocal` can still segfault because there are still no constraints on `a` and `b`. 
We could, for instance, still try to convert a `Monoid String` to a `Monoid Int`.

## Why did this even work before?
The reason why this function is safe on `Sum Int` and `Int` is that `Sum Int` is just a newtype wrapper over `Int`, and newtypes are completely erased at runtime. So a function of type `Sum Int -> b` really becomes a function of type `Int -> b` at runtime.

Thus we can safely[^5] cast it to `Int -> b` using `unsafeCoerce`.

`String` and `Int` don't have the same runtime representation, so we cannot safely cast `Dict (Monoid String)` to `Dict (Monoid Int)`.

Fortunately for us, Haskell does actually provide a way to constrain a function to types with the same runtime representation: `Coercible`!

If we adjust our function to include a `Coercible a b` constraint, we finally end up with a safe implementation that doesn't segfault!


```hs
withLocal :: forall c a b d. (c a, Coercible a b) => (c b => d) -> d
withLocal x = case cbDict of Dict -> x
    where
        caDict :: Dict (c a)
        caDict = Dict
        cbDict :: Dict (c b)
        cbDict = unsafeCoerce caDict 

λ> withLocal @Monoid @(Sum Int) @Int $ fold [1..10] <> (5 :: Int)
60
```

Note that we still had to use `unsafeCoerce` instead of `coerce`. Just because `a` can be coerced to `b`, doesn't mean GHC will allow you to cast `Dict (c a)` to `Dict (c b)`.

## Expanding
What we have so far is already pretty cool, but we can do even better.

So far, all our local instances were limited by us having to write an instance for a newtype wrapper and so our instances
could not include entirely arbitrary functions.

We couldn't, for example, declare a local function depending on a local variable and use that in an instance, because instances have to be written at the top level and therefore can't include local variables.

Fortunately, by selling a bit more of our soul to the type checker, we can work around this limitation!

At runtime, dictionaries (not `Dict`s) are really just regular data types, so there is no reason, why we shouldn't be able to fake them with `unsafeCoerce`.

Note, that `Dict` actually stores the runtime dictionary as a lifted field, so if we want to coerce to `Dict`, we need to add a layer of indirection.

```hs
data FakeDict a = FakeDict a
```

Now for the runtime dictionary, let's look at the definition of the `Eq` typeclass.

```hs
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```
`Eq` has two methods `(==)` and `(/=)`, so our datatype also has to have two fields with matching types.

```hs
data EqDict a = EqDict {
        _eq  :: a -> a -> Bool
    ,   _neq :: a -> a -> Bool
    }
```

Now we need a function to make `unsafeCoere` a little less unsafe.

```hs
withFakeDictUnsafe :: forall c d a. d -> (c => a) -> a
withFakeDictUnsafe d x = case unsafeCoerce (FakeDict d) :: Dict c of
    Dict -> x
```
This already works for simple classes like `Eq`, but right now we can't handle classes with superclasses like `Monoid`, because we have no way of storing the superclass dictionary in our fake dictionary. 

In order to include the dictionary, we have to expand our `MonoidDict` to a GADT.

```hs
data MonoidDict a where
    MonoidDict :: Semigroup a => {
        _mempty  :: a
    ,   _mappend :: a -> a -> a
    ,   _mconcat :: [a] -> a 
    } -> MonoidDict a
```

And... that's it!

We can now actually write useful functions that we couldn't just emulate by wrapping everything in a newtype.

As an example, let's consider a function that (locally) implements a `Monoid` instance for a type, which only implements `Semigroup`, based on a provided default argument for `mempty`.

```hs
withSemigroupAsMonoid :: forall a b. Semigroup a => a -> (Monoid a => b) -> b
withSemigroupAsMonoid d = withFakeDictUnsafe @(Monoid a) (MonoidDict {
        _mempty  = d
    ,   _mappend = (<>)
    ,   _mconcat = foldr (<>) d
    })
```
We couldn't use a newtype instance, since `withSemigroupAsMonoid` uses the function parameter `d` in its instance definition.

## Template Haskell
Writing all these Dict types by hand is not just annoying, but also quite dangerous because they are not automatically kept in sync with the corresponding type classes, so if one of those changes and we forget to update the Dict, the types are not actually compatible anymore.

We can use some TemplateHaskell to automate the process and (hopefully) avoid further segfaults.

My experience with TemplateHaskell is pretty limited, so I'm not going to pretend like this is a great implementation.[^6]
```hs
makeDict :: Name -> Q [Dec]
makeDict cname = do
    reify cname >>= \case -- (1)
        ClassI cdec is -> case cdec of
            ClassD cxt cname tvs _ meths -> pure [
                        dataCon [] dname tvs Nothing [ForallC (map (addSpecificity SpecifiedSpec) tvs) cxt 
                            $ RecGadtC [dname] (map methodToVarBangType meths) appliedFakeType] [] -- (2)
                    ]
                where
                    dataCon = case (meths, cxt) of
                        ([_], []) -> \cxt name tvs k [cs] ds -> NewtypeD cxt name tvs k cs ds -- (4)
                        _ -> DataD
                    dname = mkName $ nameBase cname <> "Dict" -- (3)
                    
                    methodToVarBangType (SigD n t) = (mkName ("_" <> nameBase n), Bang SourceNoUnpack NoSourceStrictness, t)
                    
                    appliedFakeType  = foldl AppT (ConT dname) (map (VarT . tyVarName) tvs)
                    appliedClassType = foldl AppT (ConT cname) (map (VarT . tyVarName) tvs)

                    addSpecificity s (PlainTV n _) = PlainTV n s
                    addSpecificity s (KindedTV n _ k) = KindedTV n s k

                    tyVarName (PlainTV n _) = n
                    tyVarName (KindedTV n _ _) = n
            _ -> fail $ "Not a class: " <> show cname
        _ -> fail $ "Not a class: " <> show cname
```
The important steps are these:

1. We get the class definition using `reify`
1. We can use that definition to construct a record GADT (`RecGadtC`) with one field for every class method.
1. The new GADT is called `<ClassName>Dict`.
1. If there is only a single method and no superclass, we construct a newtype instead. Note that this step is not just an optimization, but absolutely crucial, since a `data` constructor has one more level of indirection than a newtype, and confusing the two would lead to a segfault.

We actually get another benefit from generating our instance. Since we know, that using `withFakeDictUnsafe` with our generated `MonoidDict` is safe, we can use a (non-exported) type class and a function `withFakeDict` to constrain arguments to *safe* (i.e. generated) dictionaries.

```hs
class FakeDictFor (c :: Constraint) (d :: Type) | d -> c

withFakeDict :: forall d c a. FakeDictFor c d => d -> (c => a) -> a
withFakeDict = withFakeDictUnsafe

makeDict :: Name -> Q [Dec]
makeDict cname = do
    {- ... -}
    [d|instance FakeDictFor $(pure appliedClassType) $(pure appliedFakeType)|]
    {- ... -}
```

## Incoherence
Unfortunately, both `withLocal` and `withFakeDictUnsafe` have a pretty serious flaw.
Usually, whenever we use a typeclass method in Haskell, there are essentially two possibilities:
either *no* instance exists or there is *exactly one* and the compiler picks that one. This is called *Coherence* and is also the reason why all in-scope typeclass instances are always exported from a module.

The issue with our functions is that in case there is already an instance for the typeclass, they introduce a second instance
and break the compiler's Coherence assumption.

As an example, let's say we want to introduce a local instance for `Show Int`. 
If we do this, using `withLocal`[^7], which instance is the compiler going to pick?
Let's find out!
```hs
newtype FancyShow a = FancyShow a
instance Show a => Show (FancyShow a) where 
    show (FancyShow x) = "⭐" <> show x <> "⭐"

main = withLocal @Show @(FancyShow Int) @Int $ print (5 :: Int)
```

```bash
$ ghc unsafeCoerceDict.hs && ./unsafeCoerceDict
[1 of 1] Compiling Main             ( unsafeCoerceDict.hs, unsafeCoerceDict.o )
Linking unsafeCoerceDict ...
⭐5⭐
```
Nice! It seems like we can override instances using `withLocal`. Let's try that with optimizations
```bash
$ ghc -O unsafeCoerceDict.hs && ./unsafeCoerceDict
[1 of 1] Compiling Main             ( unsafeCoerceDict.hs, unsafeCoerceDict.o )
Linking unsafeCoerceDict ...
5
```
Oh no... optimizations can change the semantics of our program by picking the original instance, that we tried to override.
We really don't want that.

To make matters worse, there is no way to prevent incoherent usage without having to include Template Haskell at call sites, since there is no way to write a type like `Not (Eq a) => (Eq a => b) -> b`.

It's not that bad though. As long as we make sure to never try to override existing instances, we are safe.

## ImplicitParams
`withFakeDict` might remind you of a different Haskell feature, namely `ImplicitParams`.
And indeed, we can directly interoperate with `ImplicitParams` using `withFakeDict`.

An implicit parameter constraint like
```hs
f :: (?x :: Int) => Int
f = ?x + 1
```
is really just syntactic sugar for
```hs
f :: (IP "x" Int) => Int
f = ip @"x" + 1
```
where IP is a regular type class defined in `GHC.Classes`.

What's really cool about this is that we can actually define an `IP` constraint using `withFakeDict`.

So instead of
```hs
let ?x = 5 in f
```
we can write
```hs
withFakeDict @(IPDict "x" Int) (IPDict {_ip = 5}) f
```
and get the exact same behavior!

This means that `withFakeDict` is strictly more powerful than ImplicitParams, but it also shares ImplicitParams' [somewhat famous](https://chrisdone.com/posts/whats-wrong-with-implicitparams/
) [issues](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/implicit_parameters.html#implicit-parameters-and-polymorphic-recursion).

## Configurable trace
`Debug.Trace.trace` can be a very useful function for quick debugging. Want to know what this intermediate expression evaluates to? Just write it to stderr using `trace`. Want to know what values this function is called with? You can use trace for that.

```hs
f :: A -> B
f x | trace ("x = " <> show x) False = error "unreachable"
f x = ...
```

Unfortunately, `trace` is not really suited for slightly more permanent tracing, since there is no way to turn it off or to change the target it writes to.

If we wanted to improve trace, we would have to somehow pass a configuration without *actually* passing it manually to every single function. Sounds a lot like type classes to me!

Let's try that. We should also probably use `Text` instead of `String`, while we're at it.
```hs
class Trace where
    trace :: Text -> a -> a
```

To make filtering easier, we should probably also accept some kind of trace level. We could just accept an integer, but parameterizing our class over the type of the trace level is more general.

```hs
class Trace lvl where
    trace :: lvl -> Text -> a -> a
```

Cool. Functions that want to perform logging now need an additional `Trace lvl` constraint, that supplies the actual implementation.

How do we pick the implementation though? If you've paid attention so far, the answer should be obvious: We construct a fake dictionary using `withFakeDict`.

```hs
class Trace lvl where
    trace :: lvl -> Text -> a -> a

makeDict ''Trace

runTraceStderr :: (Trace lvl => a) -> a
runTraceStderr = withFakeDict (TraceDict {
        _trace _ = Debug.Trace.trace
    })

ignoreTrace :: (Trace lvl => a) -> a
ignoreTrace = withFakeDict (TraceDict {
    _trace _ _ x = x
})
```

One issue that might still come up is that someone could theoretically define a top-level
instance of `Trace lvl`, which would introduce incoherence and completely break our system. 
To prevent that, we can hide the actual implementation in a class `_Trace`, that we don't actually export.
We can then define a type synonym `Trace` that we *do* export. This way, users of our library can still reference
Trace through our type synonym, but cannot define top-level instances since instances cannot be defined for type synonyms of type classes.

```hs
class _Trace lvl where
    trace :: lvl -> Text -> a -> a

makeDict ''_Trace

type Trace = _Trace

runTraceStderr :: (Trace lvl => a) -> a
runTraceStderr = withFakeDict (Trace_Dict {
        _trace _ = Debug.Trace.trace
    })

ignoreTrace :: (Trace lvl => a) -> a
ignoreTrace = withFakeDict (Trace_Dict {
    _trace _ _ x = x
})
```

Nice! We just defined a fairly extensible little tracing library in less than 20 lines of code.

Here is why this is a good application of fake local instances

- There is no reason to manually implement the `Trace` type class, so we were able to hide it completely and
fully prevent incoherence.
- In case we made a mistake or this whole approach is fundamentally flawed and GHC decides to pick the wrong instance, nothing major breaks. (You can never be entirely sure with `unsafeCoerce` tricks like this)
- This is really easy to implement, and much less limited than most alternative approaches (Logging monads/effects, global `IORef`s, ...)

## Conclusion
Now, what did we learn from all of this?

* It is possible to transfer instances between newtypes.
* This approach can be expanded to allow the inclusion of arbitrary functions as instance methods.
* All segfaults stemming from non-unsafe functions can be avoided and it is possible to generate most boilerplate.
* Incoherence, stemming from multiple instances being available at once, is an issue.
* ImplicitParams can be emulated with local instances.
* A tiny extensible tracing library is a pretty cool application of local instances.

Does this mean, you should throw all your newtypes out of the window? No.

Does this mean, you should use the code from this article in production? Probably not. 
You should really know what you're doing if you want to use anything in production that is based on `unsafeCoerce`.
That said, if you want to try this out for yourself, the code is available in a [small library on github](https://github.com/Innf107/fakedict), including `trace`.  

Ultimately, I hope that you learned a nice little trick today. You never know when it might come in handy.


[^1]: In this particular example, most boilerplate could be eliminated by using `Sum`'s `Num` instance, but in most real
scenarios it's unfortunately not that simple.

[^2]: There are very good reasons why Haskell doesn't have local instances, but we'll get to that.

[^3]: Well... we'll see about that.

[^4]: I told you, unsafeCoerce was dangerous...

[^5]: *safe* here just means that it doesn't segfault. We'll get to other meanings of safe, don't worry!

[^6]: Feel free to [submit a pull request](https://github.com/Innf107/fakedict/tree/main/src/Fakedict/TH.hs) if you would like to write a better implementation

[^7]: I did not use `withFakeDict` here, since `Show` has a few additional methods that we would have had to implement manually

