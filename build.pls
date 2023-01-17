#!/usr/bin/env polaris

ensure("pandoc")
ensure("sassc")

let List = require("list.pls")
let Async = require("async.pls")

let listToMap(list) = {
    List.foldr(\([k, v], r) -> insert(k, v, r), #{}, list)
}

let concatMap(f, list) = match list {
    [] -> []
    (x : xs) -> f(x) ~ concatMap(f, xs)
}

let extractTags(file) = {
    let keys = lines(!grep "-Po" "(?<=<!--).+?(?=:)" file);
    let values = lines(!grep "-Po" "(?<=:).+?(?=-->)" file);
    List.zip(keys, values);
};

let escapeHTML(content) = {
    let toReplace = 
        [ ["<", "&lt;"]
        , [">", "&gt;"]
        , ["<!--.*?-->", ""]
        ]

    List.foldr(\(x, r) -> regexpReplace(List.fst(x), List.snd(x), r), content, toReplace)
};

let collectTooltips(content) = {    
    let definitionSection = match regexpMatchGroups("<!--#tooltips\s*((?:[^a]|a)+?)\s*-->", content) {
        [] -> ""
        [[_, section]] -> section
        sections -> fail("Invalid definition sections (there might be more than one?): " ~ toString(sections))
    }

    let tooltips = regexpMatchGroups("°(.+?)°\s*:\s*([^°]*)", definitionSection)

    listToMap(concatMap(\[_, def, content] -> [[def, content], [escapeHTML(def), content]], tooltips))
}

let replaceTags(tags, content) = {
    List.foldr(\(tag, r) -> replace("{{" ~ List.fst(tag) ~ "}}", List.snd(tag), r), content, tags)
};

let tooltipTemplate = !cat "templates/tooltip.html"

let replaceTooltips(tooltips, content) = {
    let addTooltipHTML(name) = {
        let tooltipContent = tooltips[name]
        if tooltipContent == null then
            fail("Invalid tooltip: °" ~ name ~ "°")
        else
            replaceTags([["name", name], ["tooltip", tooltipContent]], tooltipTemplate)
    }

    regexpTransformAll("°(.*?)°", \[_, x] -> addTooltipHTML(x), content)
}


let static_tags = [["header", !cat "static-html/header.html"], ["common-head", !cat "static-html/common-head.html"]];

let mdTags = [
        ["lang", !cat "static-html/lang-tag-start.html"]
    ,   ["/lang", !cat "static-html/lang-tag-end.html"]
    ]

let pandoc(file, tooltips) = 
    replaceTags(mdTags, !cat file) | pandoc "--katex" "--syntax-definition=highlighting/polaris.xml"

let buildPage(path, template, tags) = {
    print("Building page: " ~ path);
    let rendered_page = replaceTags(tags, !cat ("templates/" ~ template));
    
    writeFile(path, rendered_page)
};

let buildPost(name) = {
    print("Building post: " ~ name);
    let markdownPath = "md/" ~ name ~ ".md";

    let htmlPath = "posts/" ~ name ~ ".html";

    let tooltips = collectTooltips(!cat markdownPath)

    let tags = extractTags(markdownPath);

    let htmlContent = replaceTooltips(tooltips, replaceTags(tags, pandoc(markdownPath, tooltips)));

    let postTags = tags ~ static_tags ~
        [ ["title-no-code", regexpTransformAll("<code>(.+?)</code>", \groups -> groups[1], List.lookup("title", tags))]
        , ["body", htmlContent]
        ];

    let renderedPost = replaceTags(postTags, !cat "templates/post.html");

    writeFile(htmlPath, renderedPost);

    let additionalTags = 
        [ ["content", escapeHTML(htmlContent)]
        , ["link", "https://prophetlabs.de/posts/" ~ name ~ ".html"]
        ];

    additionalTags ~ postTags
};

let buildIndex(indexTags) = {
    let summaryTemplate = !cat "templates/post-summary.html";
    let summaries = List.foldr(\(tags, r) -> replaceTags(tags, summaryTemplate) ~ r, "", indexTags);
    buildPage("index.html", "index.html", static_tags ~ [["post-summaries", summaries]])
};

let buildRSS(indexTags, pubDate) = {
    let itemTemplate = !cat "templates/rss-item.xml";
    let items = List.foldr(\(tags, r) -> replaceTags(tags, itemTemplate) ~ r, "", indexTags);
    buildPage("rss.xml", "rss.xml", static_tags ~ [["rss-items", items], ["pubDate", pubDate], ["lastBuildDate", pubDate]])
};

let buildScss() = {
    let files = lines(!find "scss/post.scss" "-name" "*.scss");
    List.for(files, \file -> {
        print("Building scss file: " ~ file);
        !sassc file ("assets/" ~ (!basename "-s" ".scss" file) ~ ".css")
    })
};

async buildScss()

let posts = Async.all([
        async buildPost("unsafeCoerceDict"),
        async buildPost("coherentIP"),
    ])

async buildPage("about.html", "about.html", static_tags)

let indexTags = List.reverse(await posts)

async buildIndex(indexTags)
async buildRSS(indexTags, "05-05-2022")

