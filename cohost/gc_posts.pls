#!/usr/bin/env polaris

let for(list,f) = match list {
    [] -> ()
    (x :: xs) -> {
        f(x)
        for(xs, f)
    }
}

let contains(needle, list) = match list {
    [] -> false
    (x :: xs) -> {
        if x == needle then {
            true
        } else {
            contains(needle, xs)
        }
    }
}

chdir(scriptLocal("."))

let preferences = lines(!cat "posts.txt")
let posts = lines(!ls "posts")

for(posts, \post -> {
    if not (contains(post, preferences)) then {
        !rm "-r" "posts/${post}"
        ()
    } else {}
})

