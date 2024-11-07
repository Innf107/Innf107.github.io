{{
let title = "Newtypes Are Better Than Abstract Type Synonyms"
let date = "07 October 2024"
let pubDate = "2024-10-07"
let reddit = "Ihaventupdatedthelinkyetsorry"
}}
One of OCaml's flagship features is what they call "abstract types".
In essence, this lets programmers declare a type, even as a type synonym inside a module and, by hiding its definition in the signature, make it appear abstract to the outside world.

```ocaml
module Unix : sig
    type file_descr (* abstract *)
    val open_file : string -> file_descr
    val read_file : file_descr -> string
end = struct
    type file_descr = int
    
    let open_file file = some_internal_c_code_that_returns_an_int file
    
    let read_file file_descr = some_internal_c_code_that_takes_an_int file_descr
end
```
From the perspective of writing modules like this, this feature is great!
It means that the code inside the module can treat the type exactly as its underlying type and make whatever assumptions it needs to make without any additional ceremony because, inside the module, they *are* exactly equal!  
But from the outside, you get a fully abstract type that users can make no assumptions about other than what you expose to them via the module signature.

## Newtypes
Since Haskell's module system is significantly less powerful than OCaml's, it cannot implement abstract types in the same way. Instead, abstract types are implemented as data types or newtypes that don't export their constructor.

While this achieves roughly the same effect, it means that any code inside the module that uses the underlying type needs to wrap and unwrap the newtype constructors *everywhere*.
```hs
module Unix
  ( FileDescr -- does not export its constructor (MkFileDescr)
  , openFile
  , readFile
  )

newtype FileDescr = MkFileDescr Int

openFile :: FilePath -> IO FileDescr
openFile path = MkFileDescr (someInternalCCodeThatReturnsAnInt path)

readFile :: FileDescr -> IO String
readFIle (MkFileDescr fileDescr) = someInternalCCodeThatTakesAnInt fileDescr
```
With a small example like this, the amount of wrapping/unwrapping might not seem all that important, but in real code, these can add up and get quite annoying!
Implementing modules with abstract type synonyms is much more convenient.

## Wait that's not what you said in the title!
While I honestly believe that *inside* a module, abstract type synonyms are much more convenient than abstract types implemented through newtypes, they have one fatal flaw that already appears in OCaml and would be significantly exacerbated in a language with type classes:

Abstract type synonyms **hide too much information**.

In particular, they hide the information that an abstract type is **not** equal to another type.

To demonstrate this, let's look at a (slightly contrived) example involving GADTs in both OCaml and Haskell.
```ocaml
type _ some_gadt =
   | AnInt : int -> int some_gadt
   | SomethingElse : 'a -> 'a some_gadt

let some_pattern_match : file_descr some_gadt -> file_descr =
    function
    | SomethingElse descr -> descr
```
<span></span>
```hs
data SomeGADT a where
    AnInt :: Int -> SomeGADT Int
    SomethingElse :: a -> SomeGADT a

somePatternMatch :: SomeGADT FileDescr -> FileDescr
somePatternMatch (SomethingElse descr) = descr
```

Seems pretty harmless so far, doesn't it?

Well, if you tried to run this, you would see that only the OCaml version would complain that you haven't provided a case for `AnInt`!

And if you think about it, that seems wrong! `AnInt _` has type `int some_gadt` and you have a `file_descr some_gadt` here so it couldn't possibly be `AnInt _`, could it?

## Yes, it could!
Inside the `Unix` module, `file_descr` *is* equal to `int`, so `file_descr some_gadt` is also equal to `int some_gadt`!

The compiler needs to assume that you could have created an `AnInt _` inside that module and exposed it as a `file_descr some_gadt` that you're now passing to `some_pattern_match`.

Even worse, because the compiler cannot leak the fact that `file_descr` is an `int` internally (which would defeat the whole point of abstract types), it cannot make any assumptions about *any* other types! It also cannot assume that a `file_descr some_gadt` is not a `string some_gadt` or a `float some_gadt`, etc.

So as long as you have abstract type synonyms in the language, the compiler can never eliminate any pattern matches involving abstract types.

Newtypes avoid this issue because `FileDescr` and `Int` are *always* separate types. It's just that they can be converted seamlessly without runtime costs. [^coercible]

## What was that about type classes?
In a language with globally coherent type classes, type class instances cannot be implemented on abstract type synonyms.

Global coherence means that every type class instance for a type is equal, so it doesn't matter where you get your `Eq Int` instance from.

This is important for types like Haskell's [Map](https://www.stackage.org/haddock/lts-22.37/containers-0.6.7/Data-Map.html), a binary search tree whose structure depends on the `Ord` instance of its keys.

If you could create a `Map` with one instance of, say, `Ord Int` and then later access it with a different one, you could violate the internal invariants of the `Map` data structure.

And if you could implement type class instances on abstract type synonyms, you could do exactly that!

```ocaml
instance Ord int = ...

module Unix : sig
    type file_descr
    val some_map : (file_descr, string) map
    val some_descriptor : file_descr
end = struct
    type t = int
    let some_map = Map.from_list [(1, "a"), (2, "b"), (3, "c")]
    let some_descriptor = 2
end

instance Ord file_descr = ...

Map.lookup Unix.some_map Unix.some_descriptor
```

The `Map.from_list` call uses the `Ord int` instance defined above, but the `Map.lookup` call uses the second `Ord file_descr` instance which might be different!

Therefore, in a language like this, it would be impossible to implement any type class instances for any abstract type, including file descriptors, stacks, and many more.

And that's why newtypes are better than abstract type synonyms!

[^coercible]: One caveat you might be thinking of is that [Coercible](https://www.stackage.org/haddock/lts-22.37/base-4.18.2.1/Data-Coerce.html#t:Coercible) lets you convert a `[Int]` to a `[FileDescr]`. However, for exactly the reasons discussed here, GADTs like this always have `type role nominal` so Coercible will not allow you to convert a `SomeGADT Int` to a `SomeGADT FileDescr`.
