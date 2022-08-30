#!/usr/bin/env polaris

ensure("pandoc")
ensure("sassc")

let List = require("list.pls")
let Async = require("async.pls")

let extractTags(file) = {
    let keys = lines(!grep "-Po" "(?<=<!--).+?(?=:)" file);
    let values = lines(!grep "-Po" "(?<=:).+?(?=-->)" file);
    List.zip(keys, values);
};

let replaceTags(tags, content) = {
    List.foldr(\(tag, r) -> replace("{{" ~ List.fst(tag) ~ "}}", List.snd(tag), r), content, tags)
};

let escapeHTML(content) = {
    let toReplace = 
        [ ["<", "&lt;"]
        , [">", "&gt;"]
        , ["<!--.*?-->", ""]
        ]

    List.foldr(\(x, r) -> regexpReplace(List.fst(x), List.snd(x), r), content, toReplace)
};

let static_tags = [["header", !cat "static-html/header.html"]];

let mdTags = [
        ["lang", !cat "static-html/lang-tag-start.html"]
    ,   ["/lang", !cat "static-html/lang-tag-end.html"]
    ]

let pandoc(file) = 
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

    let tags = extractTags(markdownPath);

    let htmlContent = replaceTags(tags, pandoc(markdownPath));

    let postTags = tags ~ 
        [ ["header", !cat "static-html/header.html"]
        , ["title-no-code", regexpTransformAll("<code>(.+?)</code>", \groups -> groups[1], List.lookup("title", tags))]
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

