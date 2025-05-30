#!/usr/bin/env polaris
options {
    "--keep-build-artifacts" as keepBuildArtifacts: "Keep the intermediate build artifacts in _build"
    "--watch" as watch: "Rebuild posts if they change on disk"
    "--watch-interval" (watchInterval = "0.25s"): "Interval to check for file modifications. This accepts any format accepted by 'sleep'"
    "--clean" as clean: "Delete build outputs without actually building anything"
    "--force-cohost" as forceCohost: "Rebuild cohost posts even if the cohost/out directory exists"
}

module List = import("@std/list.pls")
module Async = import("@std/async.pls")

if clean then {
    !rm "-rf" "docs"
    !rm "-rf" "cohost/out"
    exit(0)
} else {}

# We set the locale explitly to force 'date' to use the correct month names
let $LC_TIME = "en_US.UTF-8"

chdir(scriptLocal("."))

!bash "-c" "rm -rf docs/*"
!mkdir "-p" "_build"

# :(
let lookup : forall key value. (key, List((key, value))) -> < Just(value), Nothing >
let lookup(needle, assocList) = match assocList {
    [] -> Nothing
    ((key, value) :: rest) ->
        if needle == key then
            Just(value)
        else
            lookup(needle, rest)
}

let dropLast : forall a. List(a) -> List(a)
let dropLast(list) = match list {
    [] -> []
    [last] -> []
    (first :: rest) -> first :: dropLast(rest)
}

# eh who cares about asymptotics anyway
let concatStrings : List(String) -> String
let concatStrings(strings) = match strings {
    [] -> ""
    (string :: rest) -> string ~ concatStrings(rest)
}

let forever : forall a r. (a, a -> a) -> r
let forever(initial, f) = {
    forever(f(initial), f)
}

let finally : forall a. (() -> (), () -> a) -> a
let finally(cleanup, cont) = {
    let result = try {
        cont()
    } with {
        exn -> {
            cleanup()
            raise exn
        }
    }
    cleanup()
    result
}

let lastModificationTime : String -> Number
let lastModificationTime(file) = {
    parseInt(!stat "--format=%Y" file)
}

let doesFileExist : String -> Bool
let doesFileExist(file) = {
    try { let _ = !stat file; true } with {
        CommandFailure(_) -> false
    }
}

data FloraSession = String

let withFloraSession : forall r. (FloraSession -> r) -> r
let withFloraSession(cont) = {
    let id = !bash "-c" "echo $RANDOM"
    
    !mkdir "-p" "_build/${id}"

    let cleanup() = {
        if not keepBuildArtifacts then {
            !rm "-rf" "_build/${id}"
            ()
        } else {}
    }

    finally(cleanup,\ -> cont(FloraSession(id)))
}


data FloraEnv = String

let newEnv : FloraSession -> FloraEnv
let newEnv(session) = {
    let id = !bash "-c" "echo $RANDOM"

    let env = FloraEnv("_build/${session!}/env_${id}")

    "" | flora "--write-env" (env!)

    env
}

let readVar : (FloraEnv, String) -> String
let readVar(env, name) = {
    name | flora "--read-env" (env!)
}

let writeVar : (FloraEnv, String, String) -> ()
let writeVar(env, name, value) = {
    "" | flora "--read-env" (env!) "--bind" name "--string" value "--write-env" (env!)
    ()
}

let writeBuildFile : (FloraSession, FloraEnv, String, String) -> ()
let writeBuildFile(session, env, name, value) = {
    let id = !bash "-c" "echo $RANDOM"

    let buildFile = "_build/${session!}/build_${id}"

    writeFile(buildFile, value)

    "" | flora "--read-env" (env!) "--bind" name "--file" buildFile "--write-env" (env!)
    ()
}

exception InvalidFloraEffectArguments(effect : String, arguments : List(String))
    = "Invalid arguments for effect '${effect}': ${toString(arguments)}"

exception InvalidFloraEffect(effect : String, arguments : List(String))
    = "Invalid flora effect '${effect}' performed with arguments: ${toString(arguments)}" 


let handleEffect : forall r. (FloraSession, FloraEnv, String, List(String), String -> String) -> String
let handleEffect(session, env, effect, arguments, continue) = {
    match effect {
        "dot" -> match arguments {
            [argument] -> {
                let svgOutput = argument | dot "-Tsvg"

                let preprocessed = 
                    # we would like to do styling ourselves, thank you very much
                    regexpReplace("(stroke=\"black\")|(fill=\"black\")", "", 
                    # remove the doctype that graphviz adds for some reason
                    regexpReplace("<!DOCTYPE([^a]|a)*?>", "", svgOutput))
                "<div class=\"dot-output\">${preprocessed}</div>"
            }
            _ -> raise InvalidFloraEffectArguments(effect, arguments)
        }
        _ -> raise InvalidFloraEffect(effect, arguments)
    }
}

let runFlora : (FloraSession, FloraEnv, String) -> String
let runFlora(session, env, code) = {
    let continuationFile = "_build/${session!}/flora-continuation"

    let handleSuspensionOrError(exn) = {
        let continue(argument) = 
            try !flora "--effects" continuationFile "--write-env" (env!) "--continue" "--string"  argument continuationFile with {
                exn -> handleSuspensionOrError(exn)
            }

        match exn {
            CommandFailure(details) -> match details.exitCode {
                100 -> {
                    let effect = details.stdout | jq "-r" ".effect"

                    # jq adds a \0 *after* every argument but we can only parse between them so we need to filter out the last one
                    let arguments = dropLast(split("\0", details.stdout | jq "--raw-output0" ".arguments[]"))

                    handleEffect(session, env, effect, arguments, continue)
                }
                _ -> raise exn
            }
            _ -> raise exn
        }
    }

    try code | flora "--effects" continuationFile "--read-env" (env!) "--write-env" (env!) with {
        exn -> handleSuspensionOrError(exn)
    }
}


let processMarkup : (FloraSession, FloraEnv, String) -> String
let processMarkup(session, env, contents) = {

    let floraReferences = ref []

    let processInlineFlora(matchResult) = {
        let code = match matchResult {
            [_, code] -> code
            _ -> fail("flora code regex returned invalid format")
        }

        let placeholder = !bash "-c" "echo $RANDOM"

        floraReferences := (placeholder, code) :: floraReferences!

        "[[${placeholder}]]"
    } 

    let markdown = regexpTransformAll("\\{\\{((?:a|[^a])*?)\\}\\}", processInlineFlora, contents)

    let htmlWithPlaceholders = async { markdown | pandoc "--katex" }

    let floraReferences = List.reverse(floraReferences!)

    let evaluatedFloraReferences = [(key, runFlora(session, env, value)) | let (key, value) <- floraReferences ]

    let replaceFloraReference(regexpResults) = match regexpResults {
        [_, reference] -> match lookup(reference, evaluatedFloraReferences) {
            Nothing -> fail("Flora reference '${reference}' not found in the list of evaluated flora code")
            Just(result) -> result
        }
        _ -> fail("invalid regexp format")
    }

    regexpTransformAll("\\[\\[(\\d+)\\]\\]", replaceFloraReference, await htmlWithPlaceholders)
}

let evalTemplate : (FloraSession, FloraEnv, String) -> String
let evalTemplate(session, env, file) = {
    let contents = !cat "templates/${file}"

    let interpolate(regexpResults) = match regexpResults {
        [_, code] -> runFlora(session, env, code)
        _ -> fail("invalid regexp format")
    }

    regexpTransformAll("\\{\\{((?:a|[^a])*?)\\}\\}", interpolate, contents)
}

data PostDetails = {
    title : String,
    date : String,
    localLink : String,
    fullURL : String,
    body : String
}

let buildPost : String -> PostDetails
let buildPost(name) = withFloraSession(\session -> {
    print("Building post ${name}")

    let filePath = "md/${name}.md"
    
    let fileContents = !cat filePath

    let env = newEnv(session)

    let processedPostBody = processMarkup(session, env, fileContents)

    writeBuildFile(session, env, "body", processedPostBody)
    writeVar(env, "url", "https://welltypedwit.ch/posts/${name}.html")

    let fullPostHTML = evalTemplate(session, env, "post.html")

    writeFile("docs/posts/${name}.html", fullPostHTML)

    PostDetails(
        { title = readVar(env, "title")
        , date = readVar(env, "date")
        , localLink = "/posts/${name}.html"
        , fullURL = "https://welltypedwit.ch/posts/${name}.html"
        , body = processedPostBody
        })
})

let buildIndex : List(PostDetails) -> ()
let buildIndex(posts) = withFloraSession (\session -> {
    print("Building the index")

    let renderEntry(details) = {
        let env = newEnv(session)
        writeVar(env, "title", details!.title)
        writeVar(env, "link", details!.localLink)
        writeVar(env, "date", details!.date)

        evalTemplate(session, env, "index_entry.html")
    }

    let indexEntries = concatStrings([ renderEntry(details) | let details <- posts ])

    let env = newEnv(session)
    writeVar(env, "entries", indexEntries)
    
    let fullIndex = evalTemplate(session, env, "index.html")

    writeFile("docs/index.html", fullIndex)
})

let buildRSS : List(PostDetails) -> ()
let buildRSS(posts) = withFloraSession (\session -> {
    print("Building the RSS feed")
    
    let renderItem(details) = {
        let env = newEnv(session)

        # Technically, RSS uses a version of rfc822 that allows 4 digit dates
        # but as far as i can tell, rfc5322 is just a stricter version of this
        # so this should be fine (hopefully)
        let rfc5322Date = !date "-R" "--date" (details!.date)

        writeVar(env, "title", details!.title)
        writeVar(env, "url", details!.fullURL)
        writeVar(env, "pubDate", rfc5322Date)
        writeBuildFile(session, env, "body", details!.body)

        evalTemplate(session, env, "rss_item.xml")
    }
    let items = concatStrings([ renderItem(post) | let post <- posts ])

    let buildDate = !date "-R"
    
    let env = newEnv(session)

    let lastBuildDate = !date "-R"

    writeVar(env, "lastBuildDate", lastBuildDate)
    writeBuildFile(session, env, "items", items)

    let rssFile = evalTemplate(session, env, "rss.xml")
    writeFile("docs/rss.xml", rssFile)
})

let buildPosts(posts) = {
    let postDetails = Async.all([ async buildPost(post) | let post <- posts ])

    let _ = async if watch then {
        let timestamps = [(post, lastModificationTime("md/${post}.md")) | let post <- posts]
        
        let update(timestamps) = match timestamps {
            [] -> []
            (post, time) :: rest -> {
                let newTime = lastModificationTime("md/${post}.md")
                if time < newTime then {
                    # We can't update post details in watch mode. This should be fine though since
                    # nothing so fundamental should change often and even if it does it will be fixed on the
                    # next run of the script
                    let _postDetails = buildPost(post)
                    
                    # It is important that we do *not* recalculate newTime here since there might have been
                    # changes while we built, which we want to trigger a new rebuild on the next iteration
                    (post, newTime) :: update(rest)
                } else {
                    (post, time) :: update(rest)
                }
            }
        }
        forever(timestamps, \timestamps -> {
            let newTimestamps = update(timestamps)
            !sleep watchInterval
            newTimestamps
        })
    }
    else {}

    postDetails
}

data CohostDetails = {
    title : String,
    date : String,
    localLink : String,
    fullURL : String
}

let buildCohostPost : String -> CohostDetails
let buildCohostPost(file) = withFloraSession (\session -> {
    print("Building cohost post ${file}")
    let env = newEnv(session)

    let json = "cohost/posts/${file}/post.json"


    let title = !jq "-r" ".headline" json
    let rawDate = !jq "-r" ".publishedAt" json

    let $LC_TIME = "en_US.UTF-8"
    let date = !date "--date" rawDate "+%d %B %Y"

    let rawBlocks = dropLast(split("\0", !jq "--raw-output0" ".blocks[]" json))

    let categorize(block) = {
        match (block | jq "-r" ".type") {
            "markdown" -> {
                let content = block | jq "-r" ".markdown.content"
                Markdown(content)
            }
            "ask" -> {
                let content = block | jq "-r" ".ask.content"
                Ask(content)
            }
            type_ -> fail("unsupported cohost block type: '${type_}'")
        }
    }

    let blocks = [categorize(block) | let block <- rawBlocks]
    
    let markdown = concatStrings([block ~ "\n\n" | let Markdown(block) <- blocks])

    let renderAsk(content) = {
        let env = newEnv(session)

        let rendered = content | pandoc

        writeVar(env, "content", rendered)
        evalTemplate(session, env, "ask.html")
    }

    let asks = concatStrings([renderAsk(ask) ~ "\n\n" | let Ask(ask) <- blocks])

    let body =  asks ~ (markdown | pandoc)

    let localLink = "/cohost/${file}"
    let fullURL = "https://welltypedwit.ch${localLink}"

    writeVar(env, "title", title)
    writeVar(env, "date", date)
    writeVar(env, "body", body)
    writeVar(env, "url", "https://welltypedwit.ch/cohost/${file}")

    let html = evalTemplate(session, env, "cohost_post.html")

    writeFile("cohost/out/${file}.html", html)

    CohostDetails({title=title, date=date, localLink=localLink, fullURL=fullURL})
})

let buildCohost() = withFloraSession(\session -> {
    !mkdir "-p" "cohost/out"
    !cp "cohost/maynard.png" "cohost/out/maynard.png"
    let files = lines(!cat "cohost/posts.txt")
    let details = await Async.all([async buildCohostPost(file) | let file <- files])

    print("Building the cohost index")

    let renderEntry(details) = {
        let env = newEnv(session)
        writeVar(env, "title", details.title)
        writeVar(env, "date", details.date)
        writeVar(env, "link", details.localLink)
        evalTemplate(session, env, "index_entry.html")
    }

    let env = newEnv(session)
    let entries = concatStrings([renderEntry(entry!) | let entry <- details])
    writeVar(env, "entries", entries)
    let indexFile = evalTemplate(session, env, "cohost_index.html")
    writeFile("cohost/out/index.html", indexFile)
})

!mkdir "-p" "docs"

!cp "CNAME" "docs/CNAME"
!cp "-r" "js" "docs/js"
!cp "-r" "css" "docs/css"

!mkdir "-p" "docs/posts"

let cohost = async {
    if (forceCohost || not (doesFileExist("cohost/out"))) then {
        buildCohost()
    } else {}
    !cp "-r" "cohost/out" "docs/cohost"
}

let postDetails = buildPosts(List.reverse([
        "unsafeCoerceDict",
        "coherentIP",
        "insttypes",
        "classTries",
        "newtypes",
        "value-restriction"
    ]))

let _ = await Async.all([
        async buildIndex(await postDetails),
        async buildRSS(await postDetails),
    ])

let _ = await cohost

if not (keepBuildArtifacts || watch) then {
    !rm "-rf" "_build"
    ()
} else {}

