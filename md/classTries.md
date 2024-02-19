{{
let title = "Blazingly Fast™ Type Class Resolution with Tries"
let date = "19 February 2024"
let pubDate = "2024-02-19"
let reddit = "https://www.reddit.com/r/ProgrammingLanguages/comments/1auxy31/blazingly_fast_type_class_resolution_with_tries/"
}}

One of the more fundamental hurdles when implementing type classes is instance resolution.
A programmer might write any possible number of instances for a given class, so how do we find the one we are looking for?

For the remaining examples, let's assume that our program contains the following instances

```hs
class C(a)

instance C(Int)
instance C(List(Bool))
instance C(List(String))
instance forall a. C((a, Int))
instance forall a. C((Bool, a))
```

Now, the easiest way to find an instance for a constraint, say `C((String, Int))`, is to linearly search through every single instance until one matches the constraint arguments.

For example, in this case, we would try (and fail) to match every single instance until we hit `forall a. C((a, Int))`.

Somewhat surprisingly, this is actually the strategy GHC uses.[^ghc] But with a large number of instances, this is not exactly efficient and as it turns out, with a little effort, we can do much better!

## We need to trie harder[^sorry]
If you already know what tries are, the context in which you learned about them was probably very different.

The traditional use case for tries is matching sequences of text.
The prime example here is text auto-completion. Imagine that our set of valid words consists of `["dog", "cat", "car", "candle",  "dune", "can", "current", "dose", "bat", "category"]` and the user has typed in `"cat"`. How do we find the set of valid completions?

Just like above, we could linearly walk through the set of valid words and pick out all the ones that contain the prefix `cat`. For reasons that will become apparent soon, we can visualize this flat set of all valid words as a flat tree.

{{perform dot("""digraph {
    bgcolor="transparent";
    "";
    dog;
    cat;
    car;
    candle;
    dune;
    can;
    current;
    dose;
    bat;
    category;

    "" -> dog;
    "" -> cat;
    "" -> car;
    "" -> candle;
    "" -> dune;
    "" -> can;
    "" -> current;
    "" -> dose;
    "" -> bat;
    "" -> category;
}""")}}

This is obviously pretty inefficient. If you were to try and optimize this, you might try the following: All words that contain the prefix `cat` start with a `c`, so we can group our words by their first character to immediately filter out all the ones that don't start with a `c`.

With this optimization, our tree, now represented as a map from characters to sets of words, looks like this.

{{perform dot("""digraph {
    bgcolor="transparent";
    "";
    d;
    c;
    b;
    dog;
    cat;
    car;
    candle;
    dune;
    can;
    current;
    dose;
    bat;
    category;

    "" -> d;
    "" -> c;
    "" -> b;

    d -> dog;
    d -> dune;
    d -> dose;

    c -> cat;
    c -> car;
    c -> candle;
    c -> can;
    c -> current;
    c -> category;

    b -> bat;
}""")}}

Cool! This already cut the number of words we need to search through roughly in half. 
To speed things up further, you might notice that exactly the same argument applies to the second character, as well as the third, fourth, and every one after that!

So if we apply this optimization to *every* character, effectively unrolling the words into fancy interlinked lists of characters, we end up with a **trie**!

{{perform dot("""digraph {
    bgcolor="transparent";
    "";


    "" -> {d c b};
    d -> do -> dog;
         do -> dos -> dose;
    d -> du -> dun -> dune;
    c -> ca -> cat -> cate -> categ -> catego -> categor -> category;
         ca -> car;
         ca -> can -> cand -> candl -> candle;
    c -> cu -> cur -> curr -> curre -> curren -> current;
    b -> ba -> bat;

    do[label="o"];
    dog[label="g"];
    dos[label="s"];
    dose[label="e"];
    du[label="u"];
    dun[label="n"];
    dune[label="e"];
    ca[label="a"];
    cat[label=t];
    cate[label=e];
    categ[label=g];
    catego[label=o];
    categor[label=r];
    category[label=y];
    car[label=r];
    can[label=n];
    cand[label=d];
    candl[label=l];
    candle[label=e];
    cu[label=u];
    cur[label=r];
    curr[label=r];
    curre[label=e];
    curren[label=n];
    current[label=t];
    ba[label=a];
    bat[label=t];
}""")}}

Now, instead of being linear in the number of words times their length, our search is only linear in the size of the prefix we are looking for. In practice, where words tend to share prefixes, this turns out to be *much* more efficient!

## Back to the world of types

So, how can we use tries for type classes?
Well, first of all, we need a way to unroll our complicated nested types into a linear string of atomic units that play a similar role to the characters in our word trie.

For most types, this will be pretty straightforward. We can unroll each of them into a linear string of *head constructors*. You can view this transformation as a depth-first search on our type that separates every type constructor from its arguments in every step. For example, `Either(List(String), Array((Int, Bool)))` will be unrolled into `Either(_, _) -> List(_) -> String -> Array(_) -> (_, _) -> Int -> Bool`.

We can now already construct a trie to hold our instances from above.

{{perform dot("""
digraph {
    bgcolor="transparent";

    C -> Int;
    C -> "List(_)" -> Bool;
         "List(_)" -> String;
    C -> "(\\_, \\_)" -> a -> Int2;
         "(\\_, \\_)" -> Bool2 -> a2;

    C[label = "C(_)"];
    Bool2[label="Bool"];
    a2[label="a"];
    Int2[label="Int"];
}
""")}}

You might even think that we could resolve instances by simply searching through the trie just like we did in our text prediction example above, but it's not quite that simple.

## Universally quantified complications

The issue is that some instances can contain universally quantified type variables! In our case, we have two such instances: `forall a. C(a, Int)` and `forall a. C(Bool, a)`.

Trying to resolve, say `C(Bool, Int)`, needs to try both instances and report an ambiguity error since both of them match.

More generally, at every inner node in our trie, there are either 0, 1, or 2 matching subtries that we need to check.

For example, looking up this instance in the trie above would traverse all of the red nodes.

{{perform dot("""
digraph {
    bgcolor="transparent";

    C -> Int;
    C -> "List(_)" -> Bool;
         "List(_)" -> String;
    C -> "(\\_, \\_)" -> a -> Int2 [ color = red ];
         "(\\_, \\_)" -> Bool2 -> a2 [ color = red ];

    C [ color = red, label = "C(_)" ];
    "(\\_, \\_)" [ color = red ];
    a [ color = red ];
    a2 [ color = red ];
    Bool2 [ color = red ];
    Int2 [ color = red ];

    Bool2[label="Bool"];
    a2[label="a"];
    Int2[label="Int"];
}
""")}}

This might sound like it could lead to exponential blow-up, but it actually doesn't! In the absolute worst case, instance lookup needs to traverse every path through our trie, but that is exactly equivalent to what our naive linear search strategy did so it's at least definitely not slower than that.

Okay, so we traverse every possible path through our trie that matches the types we are looking for. Is that it?

Not quite. Consider that variables might be used more than once in an instance.
For example, given an instance `forall a. C((a, a))`, we need to check that the two arguments to the tuple correspond to the same type.

To achieve this, we need to remember the first type we match a variable against along a given path.
If we come across this variable again, we *patch* the trie by inserting the (unrolled) type where the variable would be.

For example, say we have a trie with an instance for `forall a. C((a, (a, Bool)))`.
Now, matching this trie against `C((List(Int), (List(Int), Bool)))` follows this path. Once the traversal comes across the second occurrence of `a`, it locally patches the trie by replacing `a` with the blue path.

<style>
.replaced {
    stroke: var(--gray);
    fill: var(--gray);
}
</style>

{{perform dot("""
digraph {
    bgcolor="transparent";

    "C((List(Int), (List(Int), Bool)))" -> 
    "(List(Int), (List(Int), Bool))" -> 
    "List(Int) | (List(Int), Bool)" -> 
    "(List(Int), Bool)" -> 
    "List(Int) | Bool" -> 
    "Int | Bool" -> 
    Bool2 [color = "transparent"];


    "C(_)" -> Pair -> a1 -> Pair2;
                            Pair2 -> a2 -> Bool [ class = "replaced" ];
                            Pair2 -> List [ constraint = false, color = blue ];
                                     List -> Int [ color = blue ];
                                             Int -> Bool [ constraint = false; color = blue ];

    { rank = same; "C((List(Int), (List(Int), Bool)))"; "C(_)"}
    { rank = same; "(List(Int), (List(Int), Bool))"; Pair }
    { rank = same; "List(Int) | (List(Int), Bool)"; a1; subst }
    { rank = same; "(List(Int), Bool)"; Pair2 }
    { rank = same; "List(Int) | Bool"; a2; List }
    { rank = same; "Int | Bool"; Int };
    { rank = same;  Bool2; Bool }

    "C((List(Int), (List(Int), Bool)))"  [color = "transparent", fontcolor = red];
    "(List(Int), (List(Int), Bool))"  [color = "transparent", fontcolor = red];
    "List(Int) | (List(Int), Bool)"  [color = "transparent", fontcolor = red];
    "(List(Int), Bool)"  [color = "transparent", fontcolor = red];
    "List(Int) | Bool"  [color = "transparent", fontcolor = red];
    "Int | Bool"  [color = "transparent", fontcolor = red];
    Bool2 [color = "transparent", fontcolor = red];

    Bool2 [ label = Bool ]
    List [ color = blue ];
    Int [ color = blue ];
    Pair [ label = "(\\_, \\_)" ];
    Pair2 [ label = "(\\_, \\_)"];
    a1 [label = a]
    a2 [label = a, class = "replaced"]

    subst [ label = "a := List(Int)", color=transparent, fontcolor = red ]
}
""")}}

And that's it! This is enough to fully implement type class resolution in languages like Haskell, Rust or PureScript.

## What about entailment?

This can easily match simple instances, but what about instances like `forall a. C(a) => C(List(a))`?
Here, `C(List(a))` *entails* `C(a)`, such that `C(List(a))` only holds if `C(a)` holds. How do we take that into account?

Well, following GHC's footsteps, this is very easy: we don't!
Type class resolution only matches the "instance head" (i.e. the part after the `=>`), so we only need to emit a *new* constraint for `C(a)` when we land on that instance (using our substitution to find the correct type for `a`).

This might seem a bit limiting and there are more elaborate systems that can use entailment information, but in an open world setting like type classes where any module might add new instances, these can have some serious subtle downsides around modularity. 
Only matching on the instance head is a very reasonable pragmatic choice that is enough to cover most real world cases while keeping excellent performance.

## Row Polymorphism
The system we covered so far works very well for classic ML-style type systems that consist exclusively of (possibly parameterized) type constructors, but not every type system is this simple.

A common type system extension is row polymorphism, which can be used to model extensible records, polymorphic variants, and even algebraic effects. How do we include this in our trie?

The fundamental issue with row polymorphism is that the order of labels in a row does not matter. In other words, `{ x : Int, y : Bool }` and `{ y : Bool, x : Int }` are the same type (and hence, depending on the instantiations of `a` and `b`, even `{ x : Int | a }` and `{ y : Bool | b }` might be equal). 
Depending on the exact flavor of your type system, rows might be allowed to have duplicate labels. In that case, only the order of *distinct* labels is irrelevant. For example, `{ x : Int, y : Bool, x : String }` is equal to `{ y : Bool, x : Int, x : String }`, but not `{ x : String, x : Int, y : Bool }`.


How could we possibly represent type class instances for these types in a trie?

The answer is actually surprisingly easy. We can treat the record with only its labels as a head constructor, similar to what we did before.
Unlike before, we *canonicalize* rows by sorting their fields. This is especially important if we want to use the trie to detect two instances for exactly the same type (since we should probably throw an error in that case).

A beautiful property of the duplicate label semantics outlined above is that canonicalizing records with duplicate labels boils down to a stable sort since it only needs to preserve the order of duplicate labels.

For example, `{ y : Bool, x : Int }` will be canonicalized to `{ x : Int, y : Bool }` and subsequently unrolled to

{{perform dot("""
digraph {
    bgcolor="transparent";

    "{ x : \\_, y : \\_ }" -> Int -> Bool;
}
""")}}

and `{ x : Int, y : Bool, x : String | a }` becomes

{{perform dot("""
digraph {
    bgcolor="transparent";

    "{ x : \\_, x : \\_, y : \\_ | \\_ }" -> Int -> String -> Bool -> a;
}
""")}}

Now, when matching a type against a row head constructor like this, we move the types from the head constructor labels to the front in canonical order. This ensures that they are matched against the correct trie fields and keeps the remaining row to be matched against the extension variable if applicable.

For example, this is how one possible match could behave

{{perform dot("""
digraph {
    bgcolor="transparent";

    record -> "Int | String | { y : Bool }" -> "String | { y : Bool }" -> "{ y : Bool }" [ color = "transparent" ];

    rowhead -> Int -> String -> a;
    "a := { y : Bool }" [color = "transparent", fontcolor = red];

    { rank = same; record; rowhead }
    { rank = same; "Int | String | { y : Bool }"; Int }
    { rank = same; "String | { y : Bool }"; String }
    { rank = same; "{ y : Bool }"; a; "a := { y : Bool }" }

    record[label="{ z : String, y : Bool, x : Int }", shape=none, fontcolor = red];
    "Int | String | { y : Bool }"[color = "transparent", fontcolor = red]
    "String | { y : Bool }"[color = "transparent", fontcolor = red]
    "{ y : Bool }"[color = "transparent", fontcolor = red]

    rowhead[label = "{ x : \\_, z : \\_ | \\_ }"]
}

""")}}

## Overlapping instances

Usually, requiring type class resolution to come up with a single unambiguous instance is the right choice, but sometimes it makes sense to disambiguate explicitly and let the compiler choose one instance over another. Can we do this with our trie?

Yes! In fact, since we report every matching candidate, we can perform this disambiguation entirely on the result of type class resolution without ever touching our trie. The exact rules for how to do this can be a bit subtle, but one reasonable approach is the one taken by GHC, which is documented [in the fantastic GHC User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#overlapping-instances)[^userguide].

## Conclusion

Even if you don't plan on implementing a programming language with type classes any time soon (why not?), there are a few things I would like you to take away from this.

- Tries are awesome!
- Picking the right data structures is incredibly important, even in domains like type checkers where they typically don't receive that much love.
- There is a lot to be gained by applying knowledge from one area of computer science (e.g. text processing) to another (e.g. type checking)

If nothing else, this gave me a great opportunity to try out GraphViz support on my blog.


[^ghc]: To my knowledge at the time of writing anyway. They might have moved to something more efficient by now.
[^sorry]: sorry.
[^userguide]: Seriously, this is an incredibly underrated resource for nearly everything related to Haskell and GHC.
