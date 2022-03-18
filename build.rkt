#!/usr/bin/racket
#lang racket

(require racket/hash)
(require racket/path)

(define not-equal? (compose1 not equal?))
(define /= (compose1 not =))


(define/contract (read-file file) 
    (-> (or/c string? path?) string?)
    (let [(port (open-input-file file))] 
        (begin0 
            (port->string port)
            (close-input-port port) 
        )
    )
)

(define/contract (write-file content file) 
    (-> string? (or/c string? path?) void?)
    (let [(port (open-output-file file #:mode 'text #:exists 'replace))] 
        (display content port)
        (close-output-port port)
    )
)


(define/contract (pandoc file)
    (-> path-string? string?)
    (let [ (preprocessed (read-file file))]
        (let-values ([(process out in err) (subprocess #f #f #f "/usr/bin/env" "pandoc")])  
            (begin
                (display preprocessed in)
                (close-output-port in)
                (let [(result (port->string out))]
                    (subprocess-wait process)
                    (display (port->string err))
                    (when (/= 0 (subprocess-status process))
                        (error (string-append "Pandoc command failed on input '" file "'"))
                    )
                    result
                )
            )
        )
    )
)




(define/contract (render-template template fields)
    (-> string? hash? string?)
    (foldl (lambda (repl html) 
        (string-replace 
            (string-replace html (string-append "%7B%7B" (car repl) "%7D%7D") (cdr repl))
            (string-append "{{" (car repl) "}}") (cdr repl))
    )
    template
    (hash->list fields))
)

(define/contract (render-template-file template-file fields) 
    (-> path-string? hash? string?)
    (render-template (read-file template-file) fields)
)

(define (extract-metadata file)
    (define file-content (read-file file))
    (define (find-metadata name [default #f])
        (let* [ (regexp-res (regexp-match (string-append "<!--\\s*" name ":(.*?)\\s*-->") file-content))
            ]
            (if regexp-res
                (list-ref regexp-res 1)
                (if default
                    default
                    (error (string-append "post did not contain metadata and no default was given for '" name "' in " file))
                )
            )
        )
    )

    (hash 
        "title" (find-metadata "title")
        "title-no-code" (strip-html (find-metadata "title"))
        "date"  (find-metadata "date")
        "pubDate" (find-metadata "pubDate")
        "reddit" (find-metadata "reddit")
    )
)

(define/contract (extract-static-files path)
    (-> (or/c string? path?) hash?)
    (make-immutable-hash 
        (map  
            (lambda (file) 
                (cons (path->string (path-replace-extension file "")) (read-file (string-append path "/" (path->string file))))
            )
            (directory-list path)
        )
    )
)

(define/contract (strip-html input)
    (-> string? string?)
    (regexp-replace* "<.*?>" input "")
)

(define/contract (escape-html input)
    (-> string? string?)
        (string-replace 
            (string-replace 
            (regexp-replace* "<!--.*?-->" input "")
            "<" "&lt;")
        ">" "&gt;"
    )
)

(define (build-file template out-path [fields (hash)])
    (let* [ (statics (extract-static-files "static-html"))
            (content (render-template-file template (hash-union statics fields)))
        ]
        (write-file content out-path)
    )
)

(define (build-post file [in-path "md"] [out-path "posts"] [template "templates/post.html"])
    (let*(  [in-file (string-append in-path "/" (path->string file))]
            [body (pandoc in-file)]
            [meta (extract-metadata in-file)]
            [statics (extract-static-files "static-html")]
            [rendered-body (render-template body meta)]
            [content (render-template-file template (hash-union (hash "body" rendered-body) meta statics))]
        )
        (write-file content (string-append out-path "/" (path->string (path-replace-extension file "")) ".html"))
    )
)

(define (get-posts [include-path #t] [post-path "md"])
    (let* [(paths (filter (lambda (file) (equal? (path-get-extension file) #".md")) (directory-list post-path)))]
        (if include-path
            (map (lambda (file) (string-append post-path "/" (path->string file))) paths)
            paths
        )
    )
)

; TODO: Sort post-summaries by date
(define (build-index [template "templates/index.html"] [post-path "posts"] [out-path "index.html"] [fields (hash)])
    (let* [ (post-files (map (lambda (file) (string-append post-path "/" (path->string file))) (directory-list post-path)))
            (post-summaries-list (map (lambda (file) (render-template-file "templates/post-summary.html" (hash-union (extract-metadata file) (hash "url" (string-append "/" file))))) post-files))
            (post-summaries (apply string-append post-summaries-list))
        ]
        (build-file template out-path (hash "post-summaries" post-summaries))
    )
)

(define (build-rss [template "templates/rss.xml"] [out-path "rss.xml"] #:additional-fields [additional-fields (hash)])
    (let* [ (rss-items (map build-rss-item (get-posts)))
            (rendered-items (string-join rss-items "\n"))
        ]
        (build-file template out-path 
            (hash-union 
                (hash 
                    "rss-items" rendered-items
                ) 
                additional-fields))
    )
)

(define (build-rss-item source-file [template "templates/rss-item.xml"] [additional-fields (hash)])
    (let* [ (content (escape-html (pandoc source-file)))
            (meta (for/hash ([(k v) (in-hash (extract-metadata source-file))]) 
                    (values k (escape-html v))))
            (filename (file-name-from-path source-file))
            (link (string-append "http://prophetlabs.de/posts/" (path->string (path-replace-extension filename ".html"))))
        ]
        (render-template-file template
            (hash-union
                meta
                (hash
                    "content" content
                    "link" link
                )
                additional-fields))
    )
)

(for-each build-post (get-posts #f))

(build-index)
(build-rss #:additional-fields (hash "pubDate" "18 March 2022" "lastBuildDate" "18 March 2022"))
(build-file "templates/about.html" "about.html")
