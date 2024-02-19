#!/usr/bin/env polaris
options {
    "--force-rebuild" as forceRebuild: "Always rebuild every file even if it has not changed"
}

ensure("pandoc")
ensure("sassc")
ensure("jq")
ensure("dot")

module List = import("../polaris/lib/list.pls")
module Async = import("../polaris/lib/async.pls")

let idCounter = ref 0
let freshId() = {
    idCounter := idCounter! + 1
    idCounter! - 1 
}

!mkdir "-p" ".build"

data FloraEnv = String

let _ = !flora "--write-env" ".build/flora_env_base" "flora/base.flora"

let newFloraEnv() = {
    let file = ".build/flora_env_${toString(freshId())}"
    !cp ".build/flora_env_base" file
    FloraEnv(file)
}

let runFlora(flags : List(String), content) = {
    let continuationFile = ".build/flora_continuation_" ~ toString(freshId())
    let flags = List.append(["--effects", continuationFile], flags)

    let handleEffect(stdout) = {
        let effect = stdout | jq "-er" ".effect | strings"
        let result = match effect {
            "include" -> {
                let file = stdout | jq "-er" ".arguments[0] | strings"
                !cat file
            }
            "dot" -> {
                let argument = stdout | jq "-er" ".arguments[0] | strings"
                let svgOutput = argument | dot "-Tsvg"

                let preprocessed = 
                    # we would like to do styling ourselves, thank you very much
                    regexpReplace("(stroke=\"black\")|(fill=\"black\")", "", 
                    # remove the doctype that graphviz adds for some reason
                    regexpReplace("<!DOCTYPE([^a]|a)*?>", "", svgOutput))
                "<div class=\"dot-output\">${preprocessed}</div>"
            }
            _ -> fail("Unhandled effect: " ~ effect)
        }
        try {
            !flora flags "--continue" "--string" result continuationFile
        } with {
            CommandFailure(output) as error -> {
                if (output.exitCode == 100) then {
                    handleEffect(output.stdout)
                } else {
                    raise error
                }
            }
        }
    }

    try {
        let result = content | flora flags
        result
    } with {
        CommandFailure(output) as error -> {
            if (output.exitCode == 100) then {
                handleEffect(output.stdout)
            } else {
                raise error
            }
        }
    }
}

let evalFlora(env : FloraEnv, code) = {
    runFlora(["--read-env", env!], code)
}


let defineFloraVariablesRaw(env : FloraEnv, bindings) = {
    let renderBinding((name, binding)) = match binding {
        String(value) -> ["--bind", name, "--string", value]
        FilePath(path) -> ["--bind", name, "--file", path]
    }

    let definitionArgs = List.concatMap(renderBinding, bindings)
    let _ = runFlora(List.append(["--read-env", env!, "--write-env", env!], definitionArgs), "nil")
}


let defineFloraVariablesWith(env, bindings) = {
    let generateFileFor(binding) = match binding {
        File(contents) -> {
            let file = ".build/arg_" ~ toString(freshId())
            writeFile(file, contents)
            FilePath(file)
        }
        x -> x
    }

    let bindings = List.map(\(name, binding) -> (name, generateFileFor(binding)), bindings)

    defineFloraVariablesRaw(env, bindings)
}

let defineFloraVariables(env, vars) = {
    defineFloraVariablesRaw(env, List.map(\(name, str) -> (name, String(str)), vars))
}

# TODO: Move this to the standard library
let concatMap : forall a b. (a -> List(b), List(a)) -> List(b)
let concatMap(f, list) = List.foldr(\x r -> List.append(f(x), r), [], list)

let at : forall a. (Number, List(a)) -> < Just(a), Nothing >
let at(index, list) = match (index, list) {
    (_, []) -> Nothing
    (0, (x :: _)) -> Just(x)
    (index, (_ :: xs)) -> at(index - 1, xs) 
}

let lookup : forall k v. (k, List((k, v))) -> < Just(v), Nothing >
let lookup(key, list) = match List.find(\(k, v) -> k == key, list) {
    Nothing -> Nothing
    Just((_, value)) -> Just(value)
}

let doesFileExist(file) = {
    try {
        !bash "-c" ("stat '${file}' > /dev/null 2> /dev/null")
        true
    } with {
        CommandFailure(_) -> false        
    }
}

let hasChanged : { source : String, target : String } -> Bool
let hasChanged(params) = {
    if forceRebuild then 
        true
    else {
        # TODO: Use record destructuring here
        let source = params.source
        let target = params.target

        if not (doesFileExist(source)) then
            fail("hasChanged called on non-existent source file '${source}'")
        else if not (doesFileExist(target)) then
            true
        else {
            let sourceTimestamp = parseInt(!stat "-c" "%Y" source)
            let targetTimestamp = parseInt(!stat "-c" "%Y" target)

            sourceTimestamp > targetTimestamp
        }
    }
}

let extractTags : String -> List((String, String))
let extractTags(file) = {
    let keys = lines(!grep "-Po" "(?<=<!--).+?(?=:)" file);
    let values = lines(!grep "-Po" "(?<=:).+?(?=-->)" file);
    List.zip(keys, values);
};

let escapeHTML(content) = {
    let toReplace = 
        [ ("<", "&lt;")
        , (">", "&gt;")
        , ("<!--.*?-->", "")
        ]

    List.foldr(\x r -> regexpReplace(List.fst(x), List.snd(x), r), content, toReplace)
};

let collectTooltips : String -> List((String, String))
let collectTooltips(content) = {
    let definitionSection = match regexpMatchGroups("<!--#tooltips\\s*((?:[^a]|a)+?)\\s*-->", content) {
        [] -> ""
        [[_, section]] -> section
        sections -> fail("Invalid tooltip definition sections (there might be more than one?): ${toString(sections)}")
    }

    let tooltips = regexpMatchGroups("°(.+?)°\\s*:\\s*([^°]*)", definitionSection)

    concatMap(\[_, def, content] -> [(def, content), (escapeHTML(def), content)], tooltips)
}

let replaceTags : (List((String, String)), String) -> String
let replaceTags(tags, content) = {
    List.foldr(\(key, value) r -> replace("{{${key}}}", value, r), content, tags)
};

let tooltipTemplate = !cat "templates/tooltip.html"

let replaceTooltips : (List((String, String)), String) -> String 
let replaceTooltips(tooltips, content) = {
    let addTooltipHTML(name) = match lookup(name, tooltips) {
        Nothing -> fail("Invalid tooltip: °" ~ name ~ "°")
        Just(tooltipContent) -> {
            replaceTags([("name", name), ("tooltip", tooltipContent)], tooltipTemplate)
        }
    }

    regexpTransformAll("°(.*?)°", \[_, x] -> addTooltipHTML(x), content)
}


let mdTags = [
        ("lang", !cat "static-html/lang-tag-start.html")
    ,   ("/lang", !cat "static-html/lang-tag-end.html")
    ,   ("haskell-specific", !cat "static-html/haskell-specific-start.html")
    ,   ("/haskell-specific", !cat "static-html/haskell-specific-end.html")
    ]

let pandoc(contents) = {
    contents | pandoc 
        "--katex" 
        "--syntax-definition=highlighting/polaris.xml"
        "--syntax-definition=highlighting/flora.xml"
}

let evalTemplate(template, env : FloraEnv) = {
    regexpTransformAll("\\{\\{((?:a|[^a])*?)\\}\\}", \[_, x] -> evalFlora(env, x), template)
}


let buildPage(templateFile, outputFile, env) = {
    print("Building page: " ~ outputFile)
    let output = evalTemplate(!cat ("templates/" ~ templateFile), env)
    writeFile(outputFile, output)
}

let buildPost(name) = {
    # TODO: This is doing a lot of unnecessary work even if the file has not changed

    print("Building post: " ~ name);
    let markdownPath = "md/${name}.md";

    let htmlPath = "posts/${name}.html";

    let timestampPath = ".build/${name}.timestamp"

    let tooltips = collectTooltips(!cat markdownPath)

    let floraEnv = newFloraEnv()

    let evaluatedMarkdown = regexpTransformAll
                    ( "\\{\\{((?:a|[^a])*?)\\}\\}"
                    , \[_, x] -> 
                        runFlora(["--read-env", floraEnv!, "--write-env", floraEnv!], x)
                    , !cat markdownPath)
    
    let title = evalFlora(floraEnv, "title")

    let titleNoCode = regexpTransformAll("<code>(.+?)</code>", \groups -> groups[1], title)

    let htmlContent =
        if hasChanged({source=markdownPath, target=timestampPath}) then {
            !touch timestampPath

            let htmlContent = replaceTooltips(tooltips, pandoc(evaluatedMarkdown))

            writeFile(".build/${name}_body.html", htmlContent)

            htmlContent
        } else {
            print("No changes to post: ${name}")
            # If the file has not changed, we can get its content from the .build directory
            !cat (".build/${name}_body.html")
        }

    defineFloraVariablesWith
        ( floraEnv
        , [ ("body", File(htmlContent))
          , ("titleNoCode", String(titleNoCode))
          , ("link", String("https://prophetlabs.de/posts/${name}.html"))
          ])


    let postContent = evalTemplate(!cat "templates/post.html", floraEnv)

    defineFloraVariablesWith(floraEnv, [("content", File(escapeHTML(htmlContent)))])

    writeFile(htmlPath, postContent)
    floraEnv
}

let buildIndex(environments) = {
    let summaryTemplate = !cat "templates/post-summary.html";

    let summaries = List.foldr(\x r -> x ~ r, "", List.map(\env -> evalTemplate(summaryTemplate, env), environments))

    let env = newFloraEnv()

    defineFloraVariables(env, [("postSummaries", summaries)])

    buildPage("index.html", "index.html", env)
}

let buildRSS(environments, pubDate) = {

    let itemTemplate = !cat "templates/rss-item.xml";

    let items = List.foldr(\x r -> x ~ r, "", List.map(\env -> evalTemplate(itemTemplate, env), environments))

    let env = newFloraEnv()

    defineFloraVariablesWith
        (env
        , [ ("rssItems", File(items)) 
          , ("pubDate", String(pubDate))
          , ("lastBuildDate", String(pubDate))
          ])

    buildPage("rss.xml", "rss.xml", env)
}

let buildScss() = {
    let files = lines(!find "scss/" "-name" "*.scss")
    List.for(files, \file -> {
        let targetFile = "assets/${!basename "-s" ".scss" file}.css"

        if hasChanged({source = file, target = targetFile}) then {
            print("Building scss file: " ~ file);
            !sassc file targetFile
            ()
        } else {
            print("Skipping unchanged scss file: " ~ file)
        }
    })
}

let buildExtra(path) = {
    let renderedPath = "${!dirname path}/" ~ "${!basename "-s" ".md" path}.html"

    if hasChanged({source=path, target=renderedPath}) then {
        let env = newFloraEnv()

        defineFloraVariables(env, [("link", "https://prophetlabs.de/${renderedPath}")])

        let evaluatedMarkdown = regexpTransformAll
                        ( "\\{\\{((?:a|[^a])*?)\\}\\}"
                        , \[_, x] -> 
                            runFlora(["--read-env", env!, "--write-env", env!], x)
                        , !cat path)
        let renderedMarkdown = pandoc(evaluatedMarkdown)

        defineFloraVariablesWith(env, [("body", File(renderedMarkdown))])

        let rendered = evalTemplate(!cat "templates/extra.html", env)

        writeFile(renderedPath , rendered)
    }
    else {}
}

let _ = async buildScss()

let _ = Async.all([async buildExtra(file) | let file <- lines(!find "extras" "-type" "f" "-name" "*.md")])

let posts = Async.all([
        async buildPost("unsafeCoerceDict"),
        async buildPost("coherentIP"),
        async buildPost("insttypes"),
        async buildPost("classTries")
    ])

let _ = async buildPage("about.html", "about.html", newFloraEnv())

let environments = List.reverse(await posts)

let _ = async buildIndex(environments)
let _ = async buildRSS(environments, "19-02-2023")

