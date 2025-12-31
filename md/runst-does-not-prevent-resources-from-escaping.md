{{
let title = "runST does not prevent resources from escaping"
let date = "31 December 2025"
let pubDate = "2025-12-31"

let block = "
<div class='block'>
<div class='block-contents'>
<div class='block-contents-inline-code-wrapper'>
"
let blockEnd = "
</div>
</div>
</div>
"
}}
A common pattern one might see in Haskell-adjacent programming languages is the use of higher-rank types as a mechanism for delimiting the scope in which certain values can be accessed, similar to lifetimes or regions in other languages. This is sometimes called "the [`runST`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad-ST.html#v:runST) trick", after its namesake

```hs
runST :: (forall s. ST s a) -> a 
```

`runST` enables Haskell to use local mutation without leaking any of the side effects to the outside. The higher-rank function parameter ensures that this is safe, and references cannot be accessed outside the call to `runST`.

...

You know what. It's nearly 2026. Why am *I* explaining this when you could ask an LLM.[^whyllm] (This is ChatGPT. I'll spare you the rambling)

{{block}}

### 🔐 Why This Is Safe

The `s` type parameter ensures referential transparency:

Mutable references inside an `ST` block are tagged with `s`.

Because `s` is universally quantified at the call site (`forall s`), you cannot return an `STRef s ...` out of runST.

This prevents mutation from leaking into the pure world.

So you can’t do this:

```hs
bad :: STRef s Int
bad = runST (newSTRef 0)
```

The type system prevents it.

{{blockEnd}}

Okay, that `bad` snippet does indeed not compile! So clearly this must be how `ST` works (and LLMs are always correct anyway obviously).

Let's play with this a little and, let's say, write a safe wrapper for [`alloca`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Foreign-Marshal-Alloc.html#v:alloca).

`alloca` is unsafe because leaking the `Ptr` and accessing it after the end of the continuation leads to use-after-free.

If we add a type parameter to our pointer type and universally quantify over it in `allocaSafe`, then ChatGPT told us that the pointer cannot escape its continuation. So as long as we make sure there is no way to get the underlying pointer back out, this should be safe![^alloca]

```hs
-- abstract
newtype SafePtr s a = MkSafePtr (Ptr a)
type role SafePtr nominal representational -- would you have thought of this? ^^

peekSafe :: Storable a => SafePtr s a -> IO a
peekSafe (MkSafePtr pointer) = peek pointer

pokeSafe :: Storable a => SafePtr s a -> a -> IO ()
pokeSafe (MkSafePtr pointer) x = poke pointer x

allocaSafe :: Storable a => (forall s. SafePtr s a -> IO b) -> IO b
allocaSafe f = do
   pointer <- malloc
   result <- f (MkSafePtr pointer)
   free pointer
   pure result
```

If we try this, we can see that the following code *is* rightfully rejected
```hs
allocaSafe @Int \safePtr -> pure safePtr
```

<strong>

```
allocasafe.hs:24:48: error: [GHC-46956]
    • Couldn't match expected type ‘a0’
                  with actual type ‘SafePtr s Int’
        because type variable ‘s’ would escape its scope
```

</strong>

So we're safe, right?

## There exists a problem

A seemingly quite unrelated feature of Haskell and many other languages is called *existential types*. Whereas a *universally* quantified type of the form `forall a. ...` can be instantiated to use *any* possible type for `a`, an existential type of the form `exists a. ...` uses one specific type `a` but doesn't expose what it is (it only exposes that one such type "exists").

Haskell — like most other languages — doesn't directly expose an existential quantifier, but it still allows existentials in GADTs. For example, this defines a type `SomeList` that contains a list but doesn't tell us what the type of its elements is.

```hs
data SomeList where
   MkSomeList :: [a] -> SomeList
```

So, if we wrap `SafePtr` in an existential, we can hide the `s` parameter we obtained from `allocaSafe`!

```hs
data NotSoSafePtr a where
   MkNotSoSafePtr :: SafePtr s a -> NotSoSafePtr a
```

Does this mean we can...

## Use-after-free
```hs
main :: IO ()
main = do
    MkNotSoSafePtr pointer <- allocaSafe @Int \safePtr -> pure (MkNotSoSafePtr safePtr)
    pokeSafe pointer 42
```
```
$ valgrind safeptr
==230152== Memcheck, a memory error detector
[...]
==230569== Invalid write of size 8
==230569==    at 0x409C10: ghczminternal_GHCziInternalziForeignziStorable_zdfStorableInt1_info (in /home/prophet/temp/safeptr/safeptr)
==230569==  Address 0x4e69200 is 0 bytes inside a block of size 8 free'd
==230569==    at 0x484C8EF: free (vg_replace_malloc.c:989)
==230569==    by 0x409811: ghczminternal_GHCziInternalziForeignziMarshalziAlloc_free1_info (in /home/prophet/temp/safeptr/safeptr)
==230569==  Block was alloc'd at
==230569==    at 0x48497A8: malloc (vg_replace_malloc.c:446)
==230569==    by 0x40974E: ghczminternal_GHCziInternalziForeignziMarshalziAlloc_malloc_info (in /home/prophet/temp/safeptr/safeptr)
```

## So how does `ST` *actually* work?
The guarantee `ST` gives is that an `STRef` (or similar) can only be read from or written to *in the same `runST` invocation that created it*.

The higher-rank continuation doesn't directly contribute to this safety guarantee at all. It only creates a fresh type-level *tag* that uniquely identifies the `runST` invocation and links the `STRef` to it.

Leaking the `STRef` (or any other resource tagged this way) is absolutely possible. It just cannot be *accessed* after the leakage because any other `runST` call will use an `ST` monad with a different `s` variable.

## Bonus: We don't even need existentials
Using regular higher-rank types, we can encode an existential type like `exists s. T s` as a rank-2 function `forall r. (forall s. T s -> r) -> r`.

And that's still enough for a use-after-free!

```hs
newtype NotSoSafePtr a = MkNotSoSafePtr (forall r. (forall s. SafePtr s a -> r) -> r)

main :: IO ()
main = do
    MkNotSoSafePtr usePtr <- allocaSafe @Int \safePtr -> pure (MkNotSoSafePtr \f -> f safePtr)
    usePtr \pointer -> pokeSafe pointer 42    

``` 

[^whyllm]: A bit more seriously: I think this works as a decent proxy for what the average haskeller thinks. At a minimum, its explanation lines up with what I used to believe.

[^alloca]: If you're wondering why I'm defining `allocaSafe` in terms of [`malloc`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Foreign-Marshal-Alloc.html#v:malloc) and [`free`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Foreign-Marshal-Alloc.html#v:free) instead of [`alloca`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Foreign-Marshal-Alloc.html#v:alloca), it's because `alloca` actually doesn't use `malloc`! It [allocates a byte array on the garbage collected heap and then takes an unmanaged pointer to its contents](https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/src/GHC.Internal.Foreign.Marshal.Alloc.html#allocaBytesAlignedAndUnchecked) because that's faster than actually going through `malloc` and `free`. So if we leaked the pointer out of `alloca`, there would still be a risk of use-after-free but it would only trigger after a garbage collection and wouldn't be on the C heap so it would be hard to detect.