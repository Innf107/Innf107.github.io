#!/usr/bin/env polaris
options {
    "--force-rebuild" as forceRebuild: "Always rebuild every file even if it has not changed"
}

ensure("pandoc")
ensure("sassc")

module List = import("../polaris/lib/list.pls")
module Async = import("../polaris/lib/async.pls")

!mkdir "-p" ".build"

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
        !bash "-c" ("stat '" ~ file ~ "' > /dev/null 2> /dev/null")
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
            fail("hasChanged called on non-existant source file '" ~ source ~ "'")
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
    let definitionSection = match regexpMatchGroups("<!--#tooltips\s*((?:[^a]|a)+?)\s*-->", content) {
        [] -> ""
        [[_, section]] -> section
        sections -> fail("Invalid definition sections (there might be more than one?): " ~ toString(sections))
    }

    let tooltips = regexpMatchGroups("°(.+?)°\s*:\s*([^°]*)", definitionSection)

    concatMap(\[_, def, content] -> [(def, content), (escapeHTML(def), content)], tooltips)
}

let replaceTags : (List((String, String)), String) -> String
let replaceTags(tags, content) = {
    List.foldr(\(key, value) r -> replace("{{" ~ key ~ "}}", value, r), content, tags)
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


let static_tags = [("header", !cat "static-html/header.html"), ("common-head", !cat "static-html/common-head.html")];

let mdTags = [
        ("lang", !cat "static-html/lang-tag-start.html")
    ,   ("/lang", !cat "static-html/lang-tag-end.html")
    ,   ("haskell-specific", !cat "static-html/haskell-specific-start.html")
    ,   ("/haskell-specific", !cat "static-html/haskell-specific-end.html")
    ]

let pandoc(file, tooltips) = 
    replaceTags(mdTags, !cat file) | pandoc "--katex" "--syntax-definition=highlighting/polaris.xml"

let buildPage(path, template, tags) = {
    print("Building page: " ~ path);
    let rendered_page = replaceTags(tags, !cat ("templates/" ~ template));
    
    writeFile(path, rendered_page)
}

let buildPost(name) = {
    print("Building post: " ~ name);
    let markdownPath = "md/" ~ name ~ ".md";

    let htmlPath = "posts/" ~ name ~ ".html";

    let tooltips = collectTooltips(!cat markdownPath)

    let tags = extractTags(markdownPath);

    let title = match lookup("title", tags) {
        Nothing -> fail("Post '" ~ name ~ " is missing a 'title' tag")
        Just(title) -> title
    }

    let tagsToExport = 
        ("title-no-code", regexpTransformAll("<code>(.+?)</code>", \groups -> groups[1], title)) 
        :: tags


    let buildPostTags(htmlContent) = List.append(tagsToExport, List.append(static_tags,
            [ ("body", htmlContent)
            ]));

    let (htmlContent, postTags) = if hasChanged({source=markdownPath, target=htmlPath}) then {

        let htmlContent = replaceTooltips(tooltips, replaceTags(tags, pandoc(markdownPath, tooltips)));

        writeFile(".build/" ~ name ~ "_body.html", htmlContent)

        let postTags = buildPostTags(htmlContent)

        let renderedPost = replaceTags(postTags, !cat "templates/post.html");

        writeFile(htmlPath, renderedPost)

        (htmlContent, postTags)
    } else {
        print("No changes to post: " ~ name)
        # If the file has not changed, we can get its content from the .build directory
        let htmlContent = !cat (".build/" ~ name ~ "_body.html")
        (htmlContent, buildPostTags(htmlContent))
    }

    let additionalTags = 
        [ ("content", escapeHTML(htmlContent))
        , ("link", "https://prophetlabs.de/posts/" ~ name ~ ".html")
        ]

    List.append(additionalTags, postTags)
}

let buildIndex(indexTags) = {
    let summaryTemplate = !cat "templates/post-summary.html";
    let summaries = List.foldr(\tags r -> replaceTags(tags, summaryTemplate) ~ r, "", indexTags);
    buildPage("index.html", "index.html", List.append(static_tags, [("post-summaries", summaries)]))
}

let buildRSS(indexTags, pubDate) = {
    let itemTemplate = !cat "templates/rss-item.xml";
    let items = List.foldr(\tags r -> replaceTags(tags, itemTemplate) ~ r, "", indexTags);
    buildPage("rss.xml", 
              "rss.xml", 
                List.append(static_tags, 
                    [ ("rss-items", items), 
                    ("pubDate", pubDate), 
                    ("lastBuildDate", pubDate) ]))
}

let buildScss() = {
    let files = lines(!find "scss/" "-name" "*.scss")
    List.for(files, \file -> {
        let targetFile = "assets/" ~ (!basename "-s" ".scss" file) ~ ".css"

        if hasChanged({source = file, target = targetFile}) then {
            print("Building scss file: " ~ file);
            !sassc file targetFile
            ()
        } else {
            print("Skipping unchanged scss file: " ~ file)
        }
    })
}

let _ = async buildScss()

let posts = Async.all([
        async buildPost("unsafeCoerceDict"),
        async buildPost("coherentIP"),
        async buildPost("insttypes"),
    ])

let _ = async buildPage("about.html", "about.html", static_tags)

let indexTags = List.reverse(await posts)

let _ = async buildIndex(indexTags)
let _ = async buildRSS(indexTags, "26-02-2022")

