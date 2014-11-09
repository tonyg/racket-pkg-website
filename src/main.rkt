#lang racket/base

(require racket/set)
(require racket/match)
(require racket/format)
(require racket/date)
(require racket/port)
(require racket/string)
(require net/uri-codec)
(require json)
(require web-server/servlet)
(require web-server/http/id-cookie)
(require web-server/http/cookie-parse)
(require "bootstrap.rkt")
(require "html-utils.rkt")
(require "packages.rkt")
(require "sessions.rkt")

(define nav-index "Package Index")
(define nav-search "Search")

(bootstrap-navbar-header
 `(a ((href "http://www.racket-lang.org/"))
   (img ((src "/logo-and-text.png")
         (height "60")
         (alt "Racket Package Index")))))

(bootstrap-navigation `((,nav-index "/")
                        (,nav-search "/search")
                        ;; ((div (span ((class "glyphicon glyphicon-download-alt")))
                        ;;       " Download")
                        ;;  "http://download.racket-lang.org/")
                        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (request-handler named-url)
  (dispatch-rules
   [("") main-page]
   [("search") search-page]
   [("package" (string-arg)) package-page]
   [("package" (string-arg) "edit") edit-package-page]
   [("create") edit-package-page]
   [("logout") logout-page]
   ))

(module+ main
  (require "entrypoint.rkt")
  (start-service request-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-empty-source-url "git://github.com//")
(define COOKIE "pltsession")
(define recent-seconds (* 2 24 60 60)) ;; two days

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (authentication-wrap #:request request body ...)
  (authentication-wrap* #f request (lambda () body ...)))

(define-syntax-rule (authentication-wrap/require-login #:request request body ...)
  (authentication-wrap* #t request (lambda () body ...)))

(define current-session (make-parameter #f))
(define (current-email)
  (define s (current-session))
  (and s (session-email s)))

(define clear-session-cookie (make-cookie COOKIE
                                          ""
                                          #:path "/"
                                          #:expires "Thu, 01 Jan 1970 00:00:00 GMT"))

(define (authentication-wrap* require-login? request body)
  (define original-session-cookies
    (filter (lambda (c) (equal? (client-cookie-name c) COOKIE))
            (request-cookies request)))
  (define original-session-keys
    (map client-cookie-value original-session-cookies))
  ;; (log-info "Session keys from cookie: ~a" original-session-keys)
  (let redo ((session-keys original-session-keys))
    (define session (for/or ((k session-keys)) (lookup-session/touch! k)))
    ;; (log-info "session: ~a" session)
    (send/suspend/dispatch
     (lambda (embed-url)
       (if (and require-login? (not session))
           (redo (list (login-page)))
           (parameterize ((bootstrap-navbar-extension
                           (cond
                            [(not session)
                             `((a ((class "btn btn-default navbar-btn navbar-right")
                                   (href ,(embed-url (lambda (req) (redo (list (register-page)))))))
                                  "Register")
                               (a ((class "btn btn-success navbar-btn navbar-right")
                                   (href ,(embed-url (lambda (req) (redo (list (login-page)))))))
                                  "Sign in"))]
                            [else
                             `((ul ((class "nav navbar-nav navbar-right"))
                                   (li ((class "dropdown"))
                                       (a ((class "dropdown-toggle")
                                           (data-toggle "dropdown"))
                                          ,(session-email session)
                                          " "
                                          (span ((class "caret"))))
                                       (ul ((class "dropdown-menu") (role "menu"))
                                           (li (a ((href ,(named-url edit-package-page)))
                                                  (span ((class "glyphicon glyphicon-plus-sign")))
                                                  " New package"))
                                           (li (a ((href ,(tags-page-url
                                                           (list
                                                            (format "author:~a"
                                                                    (session-email session))))))
                                                  (span ((class "glyphicon glyphicon-user")))
                                                  " My packages"))
                                           (li ((class "divider"))
                                               (li (a ((href ,(named-url logout-page)))
                                                      (span ((class "glyphicon glyphicon-log-out")))
                                                      " Log out")))))))]))
                          (current-session session)
                          (bootstrap-cookies
                           (if session
                               (list (make-cookie COOKIE
                                                  (session-key session)
                                                  #:path "/"
                                                  #:secure? #t))
                               (list))))
             (body)))))))

(define (jsonp-rpc! #:sensitive? [sensitive? #f]
                    #:include-credentials? [include-credentials? #t]
                    site-relative-url
                    original-parameters)
  (define s (current-session))
  (if sensitive?
      (log-info "jsonp-rpc: sensitive request ~a" site-relative-url)
      (log-info "jsonp-rpc: request ~a params ~a~a"
                site-relative-url
                original-parameters
                (if include-credentials?
                    (if s
                        " +creds"
                        " +creds(missing)")
                    "")))
  (define stamp (~a (inexact->exact (truncate (current-inexact-milliseconds)))))
  (define callback-label (format "callback~a" stamp))
  (define extraction-expr (format "^callback~a\\((.*)\\);$" stamp))
  (let* ((parameters original-parameters)
         (parameters (if (and include-credentials? s)
                         (append (list (cons 'email (session-email s))
                                       (cons 'passwd (session-password s)))
                                 parameters)
                         parameters))
         (parameters (cons (cons 'callback callback-label) parameters)))
    (define request-url
      (string->url
       (format "https://pkgd.racket-lang.org~a?~a"
               site-relative-url
               (alist->form-urlencoded parameters))))
    (define-values (body-port response-headers) (get-pure-port/headers request-url))
    (define raw-response (port->string body-port))
    (match-define (pregexp extraction-expr (list _ json)) raw-response)
    (define reply (string->jsexpr json))
    (unless sensitive? (log-info "jsonp-rpc: reply ~a" reply))
    reply))

(define (authenticate-with-server! email password code)
  (jsonp-rpc! #:sensitive? #t
              #:include-credentials? #f
              "/jsonp/authenticate"
              (list (cons 'email email)
                    (cons 'passwd password)
                    (cons 'code code))))

(define (login-page [error-message #f])
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response "Login"
                         `(form ((class "form-horizontal")
                                 (method "post")
                                 (action ,(embed-url process-login-credentials))
                                 (role "form"))
                           (div ((class "form-group"))
                                (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                        (for "email")) "Email address:")
                                (div ((class "col-sm-5"))
                                     (input ((class "form-control")
                                             (type "email")
                                             (name "email")
                                             (value "")
                                             (id "email")))))
                           (div ((class "form-group"))
                                (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                        (for "password")) "Password:")
                                (div ((class "col-sm-5"))
                                     (input ((class "form-control")
                                             (type "password")
                                             (name "password")
                                             (value "")
                                             (id "password")))))
                           (div ((class "form-group"))
                                (div ((class "col-sm-offset-4 col-sm-5"))
                                     (a ((href ,(embed-url (lambda (req) (register-page)))))
                                        "Need to reset your password?")))
                           ,@(maybe-splice
                              error-message
                              `(div ((class "form-group"))
                                (div ((class "col-sm-offset-4 col-sm-5"))
                                     (div ((class "alert alert-danger"))
                                          (p ,error-message)))))
                           (div ((class "form-group"))
                                (div ((class "col-sm-offset-4 col-sm-5"))
                                     (button ((type "submit")
                                              (class "btn btn-primary"))
                                             "Log in"))))
                         ))))

(define (process-login-credentials request)
  (define-form-bindings request (email password))
  (if (or (equal? (string-trim email) "")
          (equal? (string-trim password) ""))
      (login-page "Please enter your email address and password.")
      (match (authenticate-with-server! email password "")
        ["wrong-code"
         (login-page "Something went awry; please try again.")]
        [(or "emailed" #f)
         (summarise-code-emailing "Incorrect password, or nonexistent user." email)]
        [else
         (create-session! email password)])))

(define (register-page #:email [email ""]
                       #:code [code ""]
                       #:error-message [error-message #f])
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response "Register/Reset Account"
                         #:title-element ""
                         `(div
                           (h1 "Got a registration or reset code?")
                           (p "Great! Enter it below, with your chosen password, to log in.")
                           (form ((class "form-horizontal")
                                  (method "post")
                                  (action ,(embed-url apply-account-code))
                                  (role "form"))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "email")) "Email address:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "email")
                                                   (name "email")
                                                   (value ,email)
                                                   (id "email")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "code")) "Code:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "text")
                                                   (name "code")
                                                   (value ,code)
                                                   (id "code")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "password")) "Password:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "password")
                                                   (name "password")
                                                   (value "")
                                                   (id "password")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "password")) "Confirm password:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "password")
                                                   (name "confirm_password")
                                                   (value "")
                                                   (id "confirm_password")))))
                                 ,@(maybe-splice
                                    error-message
                                    `(div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (div ((class "alert alert-danger"))
                                                (p ,error-message)))))
                                 (div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (button ((type "submit")
                                                    (class "btn btn-primary"))
                                                   "Continue")))))
                         `(div
                           (h1 "Need a code?")
                           (p "Enter your email address below, and we'll send you one.")
                           (form ((class "form-horizontal")
                                  (method "post")
                                  (action ,(embed-url notify-of-emailing))
                                  (role "form"))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "email")) "Email address:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "email")
                                                   (name "email_for_code")
                                                   (value "")
                                                   (id "email_for_code")))))
                                 (div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (button ((type "submit")
                                                    (class "btn btn-primary"))
                                                   "Email me a code")))))))))

(define (apply-account-code request)
  (define-form-bindings request (email code password confirm_password))
  (define (retry msg)
    (register-page #:email email
                   #:code code
                   #:error-message msg))
  (cond
   [(equal? (string-trim email) "")
    (retry "Please enter your email address.")]
   [(equal? (string-trim code) "")
    (retry "Please enter the code you received in your email.")]
   [(not (equal? password confirm_password))
    (retry "Please make sure the two password fields match.")]
   [(equal? (string-trim password) "")
    (retry "Please enter a password.")]
   [else
    (match (authenticate-with-server! email password code)
      ["wrong-code"
       (retry "The code you entered was incorrect. Please try again.")]
      [(or "emailed" #f)
       (retry "Something went awry; you have been emailed another code. Please check your email.")]
      [else
       ;; The email and password combo we have been given is good to go.
       ;; Set a cookie and consider ourselves logged in.
       (create-session! email password)])]))

(define (notify-of-emailing request)
  (define-form-bindings request (email_for_code))
  (authenticate-with-server! email_for_code "" "") ;; TODO check result?
  (summarise-code-emailing "Account registration/reset code emailed" email_for_code))

(define (summarise-code-emailing reason email)
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response reason
                         `(p
                           "We've emailed an account registration/reset code to "
                           (code ,email) ". Please check your email and then click "
                           "the button to continue:")
                         `(a ((class "btn btn-primary")
                              (href ,(embed-url (lambda (req) (register-page)))))
                           "Enter your code")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (package-link package-name)
  (define package-name-str (~a package-name))
  `(a ((href ,(named-url package-page package-name-str))) ,package-name-str))

(define (doc-destruct doc)
  (match doc
    [(list _ n u) (values n u)]
    [(list _ n) (values n #f)]))

(define (doc-link doc)
  (define-values (docset-name docset-url) (doc-destruct doc))
  (if docset-url
      (buildhost-link docset-url docset-name)
      `(del ,docset-name)))

(define (tags-page-url tags)
  (format "~a?~a"
          (named-url search-page)
          (alist->form-urlencoded (list (cons 'tags (string-join tags))))))

(define (author-link author-name)
  `(a ((href ,(tags-page-url (list (format "author:~a" author-name))))) ,author-name))

(define (tag-link tag-name)
  `(a ((href ,(tags-page-url (list tag-name)))) ,tag-name))

(define (buildhost-link #:attributes [attributes '()] url-suffix label)
  `(a (,@attributes
       (href ,(format "http://pkg-build.racket-lang.org/~a" url-suffix))) ,label))

(define (authors-list authors)
  `(ul ((class "authors")) ,@(for/list ((author authors)) `(li ,(author-link author)))))

(define (package-links #:pretty? [pretty? #t] package-names)
  (if (and pretty? (null? (or package-names '())))
      `(span ((class "packages none")) "None")
      `(ul ((class "list-inline packages"))
        ,@(for/list ((p package-names)) `(li ,(package-link p))))))

(define (doc-links docs)
  `(ul ((class "list-inline doclinks"))
    ,@(for/list ((doc (or docs '()))) `(li ,(doc-link doc)))))

(define (tag-links tags)
  `(ul ((class "list-inline taglinks")) ,@(for/list ((tag (or tags '()))) `(li ,(tag-link tag)))))

(define (utc->string utc)
  (if utc
      (string-append (date->string (seconds->date utc #f) #t) " (UTC)")
      "N/A"))

(define (package-summary-table package-names)
  (define now (/ (current-inexact-milliseconds) 1000))
  `(table
    ((class "packages sortable"))
    (tr
     (th "Package")
     (th "Description")
     (th "Build"))
    ,@(maybe-splice (null? package-names)
                    `(tr (td ((colspan "3"))
                             (div ((class "alert alert-info"))
                                  "No packages found."))))
    ,@(for/list ((package-name package-names))
        (define pkg (package-detail package-name))
        `(tr
          (td (h2 ,(package-link package-name))
              ,(authors-list (@ pkg authors))
              ,@(maybe-splice
                 (< (- now (or (@ pkg last-updated) 0)) recent-seconds)
                 `(span ((class "label label-info")) "Updated"))
              )
          (td (p ,(@ pkg description))
              ,@(maybe-splice
                 (pair? (@ pkg build docs))
                 `(div
                   (span ((class "doctags-label")) "Docs: ")
                   ,(doc-links (@ pkg build docs))))
              ,@(maybe-splice
                 (pair? (@ pkg tags))
                 `(div
                   (span ((class "doctags-label")) "Tags: ")
                   ,(tag-links (@ pkg tags)))))
          ,(cond
            [(@ pkg build failure-log)
             `(td ((class "build_red"))
               ,(buildhost-link (@ pkg build failure-log) "fails"))]
            [(and (@ pkg build success-log)
                  (@ pkg build dep-failure-log))
             `(td ((class "build_yellow"))
               ,(buildhost-link (@ pkg build success-log)
                                "succeeds")
               " with "
               ,(buildhost-link (@ pkg build dep-failure-log)
                                "dependency problems"))]
            [(@ pkg build success-log)
             `(td ((class "build_green"))
               ,(buildhost-link (@ pkg build success-log) "succeeds"))]
            [else
             `(td)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-page request)
  (parameterize ((bootstrap-active-navigation nav-index))
    (define package-name-list (package-search "" '((main-distribution #f))))
    (authentication-wrap
     #:request request
     (bootstrap-response "Racket Package Index"
                         #:title-element ""
                         `(div ((class "jumbotron"))
                           (h1 "Racket Package Index")
                           (p "These are the packages available via the "
                              (a ((href "http://docs.racket-lang.org/pkg/getting-started.html"))
                                 "Racket package system") ".")
                           (p "Simply run " (kbd "raco pkg install " (var "package-name"))
                              " to install a package.")
                           (p ((class "text-center"))
                              (span ((class "package-count")) ,(~a (length package-name-list)))
                              " packages in the index.")
                           (form ((role "form")
                                  (action ,(named-url search-page)))
                                 (div ((class "form-group"))
                                      (input ((class "form-control")
                                              (type "text")
                                              (placeholder "Search packages")
                                              (name "q")
                                              (value "")
                                              (id "q"))))
                                 ))
                         (package-summary-table package-name-list)))))

(define (logout-page request)
  (parameterize ((bootstrap-cookies (list clear-session-cookie)))
    (when (current-session) (destroy-session! (session-key (current-session))))
    (bootstrap-redirect (named-url main-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-status buildhost-url str label-type glyphicon)
  `(p ((class "build-status"))
    "Build status: "
    ,(buildhost-link buildhost-url
                     `(span ((class ,(format "label label-~a" label-type)))
                       (span ((class ,(format "glyphicon glyphicon-~a" glyphicon))))
                       " " ,str))))

(define (package-page request package-name-str)
  (authentication-wrap
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (define default-version (hash-ref (or (@ pkg versions) (hash)) 'default (lambda () #f)))
   (if (not pkg)
       (bootstrap-response #:code 404
                           #:message #"No such package"
                           "Package not found"
                           `(div "The package " (code ,package-name-str) " does not exist."))
       (bootstrap-response (~a package-name)
                           #:title-element ""
                           `(div ((class "jumbotron"))
                             (h1 ,(~a package-name))
                             (p ,(@ pkg description))
                             ,(cond
                               [(@ pkg build failure-log)
                                (build-status (@ pkg build failure-log)
                                              "failed" "danger" "fire")]
                               [(and (@ pkg build success-log)
                                     (@ pkg build dep-failure-log))
                                (build-status (@ pkg build dep-failure-log)
                                              "problems" "warning" "question-sign")]
                               [(@ pkg build success-log)
                                (build-status (@ pkg build success-log)
                                              "ok" "success" "ok")]
                               [else
                                ""])
                             (div ,@(let ((docs (or (@ pkg build docs) '())))
                                      (match docs
                                        [(list)
                                         `()]
                                        [(list doc)
                                         (define-values (n u) (doc-destruct doc))
                                         (list (buildhost-link
                                                #:attributes `((class "btn btn-success btn-lg"))
                                                u
                                                "Documentation"))]
                                        [_
                                         `((button ((class "btn btn-success btn-lg dropdown-toggle")
                                                    (data-toggle "dropdown"))
                                                   "Documentation "
                                                   (span ((class "caret"))))
                                           (ul ((class "dropdown-menu")
                                                (role "menu"))
                                               ,@(for/list ((doc docs)) `(li ,(doc-link doc)))))]))

                                  ;; Heuristic guess as to whether we should present a "browse"
                                  ;; link or a "download" link.
                                  " "
                                  ,(if (equal? (@ default-version source)
                                               (@ default-version source_url))
                                       `(a ((class "btn btn-default btn-lg")
                                            (href ,(@ default-version source_url)))
                                         (span ((class "glyphicon glyphicon-download")))
                                         " Snapshot")
                                       `(a ((class "btn btn-default btn-lg")
                                            (href ,(@ default-version source_url)))
                                         (span ((class "glyphicon glyphicon-link")))
                                         " Code"))

                                  ,@(maybe-splice
                                     (member (current-email) (or (@ pkg authors) '()))
                                     " "
                                     `(a ((class "btn btn-info btn-lg")
                                          (href ,(named-url edit-package-page package-name-str)))
                                       (span ((class "glyphicon glyphicon-edit")))
                                       " Edit this package"))
                                  ))

                           (if (@ pkg _LOCALLY_MODIFIED_)
                               `(div ((class "alert alert-warning")
                                      (role "alert"))
                                 (span ((class "glyphicon glyphicon-exclamation-sign")))
                                 " This package has been modified since the package index was last rebuilt."
                                 " The next index refresh is scheduled for "
                                 ,(utc->string (/ (next-fetch-deadline) 1000)) ".")
                               "")

                           (if (@ pkg checksum-error)
                               `(div ((class "alert alert-danger")
                                      (role "alert"))
                                 (span ((class "label label-danger"))
                                       "Checksum error")
                                 " The package checksum does not match"
                                 " the package source code.")
                               "")

                           `(table ((class "package-details"))
                             (tr (th "Authors")
                                 (td ,(authors-list (@ pkg authors))))
                             (tr (th "Documentation")
                                 (td ,(doc-links (@ pkg build docs))))
                             (tr (th "Tags")
                                 (td ,(tag-links (@ pkg tags))))
                             (tr (th "Last updated")
                                 (td ,(utc->string (@ pkg last-updated))))
                             (tr (th "Ring")
                                 (td ,(~a (or (@ pkg ring) "N/A"))))
                             (tr (th "Conflicts")
                                 (td ,(package-links (@ pkg conflicts))))
                             (tr (th "Dependencies")
                                 (td ,(package-links (@ pkg dependencies))))
                             (tr (th "Most recent build results")
                                 (td (ul ((class "build-results"))
                                         ,@(maybe-splice
                                            (@ pkg build success-log)
                                            `(li "Compiled successfully: "
                                              ,(buildhost-link (@ pkg build success-log) "transcript")))
                                         ,@(maybe-splice
                                            (@ pkg build failure-log)
                                            `(li "Compiled unsuccessfully: "
                                              ,(buildhost-link (@ pkg build failure-log) "transcript")))
                                         ,@(maybe-splice
                                            (@ pkg build conflicts-log)
                                            `(li "Conflicts: "
                                              ,(buildhost-link (@ pkg build conflicts-log) "details")))
                                         ,@(maybe-splice
                                            (@ pkg build dep-failure-log)
                                            `(li "Dependency problems: "
                                              ,(buildhost-link (@ pkg build dep-failure-log) "details")))
                                         )))
                             ,@(let* ((vs (or (@ pkg versions) (hash)))
                                      (empty-checksum "9f098dddde7f217879070816090c1e8e74d49432")
                                      (vs (for/hash (((k v) (in-hash vs))
                                                     #:when (not (equal? (@ v checksum)
                                                                         empty-checksum)))
                                            (values k v))))
                                 (maybe-splice
                                  (not (hash-empty? vs))
                                  `(tr (th "Versions")
                                    (td (table ((class "package-versions"))
                                               (tr (th "Version")
                                                   (th "Source")
                                                   (th "Checksum"))
                                               ,@(for/list
                                                     (((version-sym v) (in-hash vs)))
                                                   `(tr
                                                     (td ,(~a version-sym))
                                                     (td (a ((href ,(@ v source_url)))
                                                            ,(@ v source)))
                                                     (td ,(@ v checksum)))))))))
                             (tr (th "Last checked")
                                 (td ,(utc->string (@ pkg last-checked))))
                             (tr (th "Last edited")
                                 (td ,(utc->string (@ pkg last-edit))))
                             (tr (th "Modules")
                                 (td (ul ((class "module-list"))
                                         ,@(for/list ((mod (or (@ pkg modules) '())))
                                             (match-define (list kind path) mod)
                                             `(li ((class ,kind)) ,path)))))
                             )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct draft-package (old-name name description authors tags versions) #:transparent)

(define (edit-package-page request [package-name-str ""])
  (authentication-wrap/require-login
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (cond
    [(and pkg (not (member (current-email) (or (@ pkg authors) '()))))
     ;; Not ours. Show it instead.
     (bootstrap-redirect (named-url package-page package-name-str))]
    [(not pkg)
     ;; Doesn't exist.
     (package-form #f (draft-package ""
                                     package-name-str
                                     ""
                                     (list (current-email))
                                     '()
                                     `(("default" ,default-empty-source-url))))]
    [else
     (package-form #f
                   (draft-package package-name-str
                                  package-name-str
                                  (@ pkg description)
                                  (@ pkg authors)
                                  (@ pkg tags)
                                  (for/list (((ver info) (in-hash (@ pkg versions))))
                                    (list (symbol->string ver) (@ info source)))))])))

(define (package-source-option source-type value label)
  `(option ((value ,value)
            ,@(maybe-splice (equal? source-type value) '(selected "selected")))
    ,label))

(define (put-default-first alist)
  (define default (assoc "default" alist))
  (cons default (remove default alist)))

(define (package-form error-message draft)
  (send/suspend/dispatch
   (lambda (embed-url)

     (define (build-versions-table)
       `(table ((class "package-versions"))
         (tr (th "Version")
             (th "Source"))
         ,@(for/list ((v (put-default-first
                          (draft-package-versions draft))))
             (match-define (list version source) v)
             (define (control-name c) (format "version__~a__~a" version c))
             (define (group-name c) (format "version__~a__~a__group" version c))
             (define (textfield name label value [placeholder ""])
               `(div ((id ,(group-name name))
                      (class "row"))
                 ,@(maybe-splice
                    label
                    `(div ((class "col-sm-3"))
                      (label ((class "control-label")
                              (for ,(control-name name)))
                             ,label)))
                 (div ((class ,(if label "col-sm-9" "col-sm-12")))
                      (input ((class "form-control")
                              (type "text")
                              (name ,(control-name name))
                              (id ,(control-name name))
                              (placeholder ,placeholder)
                              (value ,value))))))
             (define-values (source-type simple-url g-host g-user g-project g-branch)
               (match source
                 [(pregexp #px"github://github\\.com/([^/]*)/([^/]*)(/([^/]*)/?)?"
                           (list _ u p _ b))
                  (values "github" "" "github.com" u p (if (equal? b "master") "" (or b #f)))]
                 [(pregexp #px"git://([^/]*)/([^/]*)/([^/]*)(/([^/]*)/?)?"
                           (list _ h u p _ b))
                  (values "git" "" h u p (if (equal? b "master") "" (or b "")))]
                 [_
                  (values "simple" source "" "" "" "")]))
             `(tr
               (td ,version
                   ,@(maybe-splice
                      (not (equal? version "default"))
                      " "
                      `(button ((class "btn btn-danger btn-xs")
                                (type "submit")
                                (name "action")
                                (value ,(control-name "delete")))
                        (span ((class "glyphicon glyphicon-trash"))))))
               (td (div ((class "row"))
                        (div ((class "col-sm-3"))
                             (div ((id ,(group-name "type")))
                                  (select ((class "package-version-source-type")
                                           (data-packageversion ,version)
                                           (name ,(control-name "type")))
                                          ,(package-source-option source-type
                                                                  "github"
                                                                  "Github Repository")
                                          ,(package-source-option source-type
                                                                  "git"
                                                                  "Git Repository")
                                          ,(package-source-option source-type
                                                                  "simple"
                                                                  "Simple URL"))))
                        (div ((id ,(group-name "fields"))
                              (class "col-sm-9"))
                             (div ((id ,(group-name "urlpreview"))
                                   (class "row"))
                                  (div ((class "col-sm-3"))
                                       (label ((class "control-label")) "URL preview"))
                                  (div ((class "col-sm-9"))
                                       (span ((class "form-control disabled")
                                              (disabled "disabled")
                                              (id ,(control-name "urlpreview"))))))
                             ,(textfield "simple_url" #f simple-url)
                             ,(textfield "g_host" "Repo Host" g-host)
                             ,(textfield "g_user" "Repo User" g-user)
                             ,(textfield "g_project" "Repo Project" g-project)
                             ,(textfield "g_branch" "Repo Branch" g-branch "master")
                             )))))
         (tr (td ((colspan "2"))
                 (div ((class "form-inline"))
                      (input ((class "form-control")
                              (type "text")
                              (name "new_version")
                              (id "new_version")
                              (placeholder "x.y.z")
                              (value "")))
                      " "
                      (button ((class "btn btn-success btn-xs")
                               (type "submit")
                               (name "action")
                               (value "add_version"))
                              (span ((class "glyphicon glyphicon-plus-sign")))
                              " Add new version"))))
         ))

     (parameterize ((bootstrap-page-scripts '("/editpackage.js")))
       (define old-name (draft-package-old-name draft))
       (define has-old-name? (not (equal? old-name "")))
       (bootstrap-response (if has-old-name?
                               (format "Editing package ~a" old-name)
                               "Creating a new package")
                           (if error-message
                               `(div ((class "alert alert-danger"))
                                 (span ((class "glyphicon glyphicon-exclamation-sign")))
                                 " " ,error-message)
                               "")
                           `(form ((id "edit-package-form")
                                   (method "post")
                                   (action ,(embed-url (update-draft draft)))
                                   (role "form"))
                             (div ((class "container")) ;; TODO: remove??
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "name")
                                                    (class "control-label"))
                                                   "Package Name")
                                            (input ((class "form-control")
                                                    (type "text")
                                                    (name "name")
                                                    (id "name")
                                                    (value ,(~a (draft-package-name draft))))))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "tags")
                                                    (class "control-label"))
                                                   "Package Tags (space-separated)")
                                            (input ((class "form-control")
                                                    (type "text")
                                                    (name "tags")
                                                    (id "tags")
                                                    (value ,(string-join
                                                             (draft-package-tags draft)))))))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "description")
                                                    (class "control-label"))
                                                   "Package Description")
                                            (textarea ((class "form-control")
                                                       (name "description")
                                                       (id "description"))
                                                      ,(draft-package-description draft)))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "authors")
                                                    (class "control-label"))
                                                   "Author email addresses (one per line)")
                                            (textarea ((class "form-control")
                                                       (name "authors")
                                                       (id "authors"))
                                                      ,(string-join (draft-package-authors draft)
                                                                    "\n"))))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-12"))
                                            (label ((class "control-label"))
                                                   "Package Versions & Sources")
                                            ,(build-versions-table)))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-12"))
                                            ,@(maybe-splice
                                               has-old-name?
                                               `(a ((class "btn btn-danger pull-right")
                                                    (href ,(embed-url
                                                            (confirm-package-deletion old-name))))
                                                 (span ((class "glyphicon glyphicon-trash")))
                                                 " Delete package")
                                               " ")
                                            (button ((type "submit")
                                                     (class "btn btn-primary")
                                                     (name "action")
                                                     (value "save_changes"))
                                                    (span ((class "glyphicon glyphicon-save")))
                                                    " Save changes")
                                            ,@(maybe-splice
                                               has-old-name?
                                               " "
                                               `(a ((class "btn btn-default")
                                                    (href ,(named-url package-page old-name)))
                                                 "Cancel changes and return to package page"))))))
                           )))))

(define ((confirm-package-deletion package-name-str) request)
  (send/suspend
   (lambda (k-url)
     (bootstrap-response "Confirm Package Deletion"
                         `(div ((class "confirm-package-deletion"))
                           (h2 ,(format "Delete ~a?" package-name-str))
                           (p "This cannot be undone.")
                           (a ((class "btn btn-default")
                               (href ,k-url))
                              "Confirm deletion")))))
  (jsonp-rpc! "/jsonp/package/del" `((pkg . ,package-name-str)))
  (delete-package! (string->symbol package-name-str))
  (bootstrap-redirect (named-url main-page)))

(define ((update-draft draft0) request)
  (define draft (read-draft-form draft0 (request-bindings request)))
  (define-form-bindings request (action new_version))
  (match action
    ["save_changes"
     (if (save-draft! draft)
         (bootstrap-redirect (named-url package-page (~a (draft-package-name draft))))
         (package-form "Save failed."
                       ;; ^ TODO: This is the worst error message.
                       ;;         Right up there with "parse error".
                       draft))]
    ["add_version"
     (if (assoc new_version (draft-package-versions draft))
         (package-form (format "Could not add version ~a, as it already exists." new_version)
                       draft)
         (package-form #f (struct-copy draft-package draft
                                       [versions (cons (list new_version default-empty-source-url)
                                                       (draft-package-versions draft))])))]
    [(regexp #px"^version__(.*)__delete$" (list _ version))
     (package-form #f (struct-copy draft-package draft
                                   [versions (filter (lambda (v)
                                                       (not (equal? (car v) version)))
                                                     (draft-package-versions draft))]))]))

(define (read-draft-form draft bindings)
  (define (g key d)
    (cond [(assq key bindings) => cdr]
          [else d]))
  (define (read-version-source version)
    (define (vg name d)
      (g (string->symbol (format "version__~a__~a" version name)) d))
    (define type (vg 'type "simple"))
    (define simple_url (vg 'simple_url ""))
    (define g_host (vg 'g_host ""))
    (define g_user (vg 'g_user ""))
    (define g_project (vg 'g_project ""))
    (define g_branch0 (vg 'g_branch ""))
    (define g_branch (if (equal? g_branch0 "") "master" g_branch0))
    (match type
      ["github" (format "github://github.com/~a/~a/~a" g_user g_project g_branch)]
      ["git"    (format "git://~a/~a/~a/~a" g_host g_user g_project g_branch)]
      ["simple" simple_url]))
  (struct-copy draft-package draft
               [name (g 'name (draft-package-old-name draft))]
               [description (g 'description "")]
               [authors (string-split (g 'authors ""))]
               [tags (string-split (g 'tags ""))]
               [versions (for/list ((old (draft-package-versions draft)))
                           (match-define (list version _) old)
                           (list version
                                 (read-version-source version)))]))

(define (added-and-removed old new)
  (define old-set (list->set (or old '())))
  (define new-set (list->set new))
  (values (set->list (set-subtract new-set old-set))
          (set->list (set-subtract old-set new-set))))

(define (save-draft! draft)
  (match-define (draft-package old-name name description authors tags versions/default) draft)
  (define default-version (assoc "default" versions/default))
  (define source (cadr default-version))
  (define versions (remove default-version versions/default))

  (define old-pkg (package-detail (string->symbol old-name)))

  (define-values (added-tags removed-tags)
    (added-and-removed (@ old-pkg tags) tags))
  (define-values (added-authors removed-authors)
    (added-and-removed (or (@ old-pkg authors) (list (current-email))) authors))

  (define old-versions-map (or (@ old-pkg versions) (hash)))
  (define changed-versions
    (for/fold ((acc '())) ((v versions))
      (match-define (list version-str new-source) v)
      (define version-sym (string->symbol version-str))
      (define old-source (@ (@ref old-versions-map version-sym) source))
      (if (equal? old-source new-source)
          acc
          (cons v acc))))
  (define removed-versions
    (for/list ((k (in-hash-keys old-versions-map))
               #:when (not (assoc (symbol->string k) versions/default))) ;; NB versions/default !
      (symbol->string k)))

  ;; name, description, and default source are updateable via /jsonp/package/modify.
  ;; tags are added and removed via /jsonp/package/tag/add and .../del.
  ;; authors are added and removed via /jsonp/package/author/add and .../del.
  ;; versions other than default are added and removed via /jsonp/package/version/add and .../del.
  (and (or (equal? old-name name)
           ;; Don't let renames stomp on existing packages
           (not (package-detail (string->symbol name))))
       (jsonp-rpc! "/jsonp/package/modify" `((pkg . ,old-name)
                                             (name . ,name)
                                             (description . ,description)
                                             (source . ,source)))
       (andmap (lambda (t) (jsonp-rpc! "/jsonp/package/tag/add" `((pkg . ,name) (tag . ,t))))
               added-tags)
       (andmap (lambda (t) (jsonp-rpc! "/jsonp/package/tag/del" `((pkg . ,name) (tag . ,t))))
               removed-tags)
       (andmap (lambda (a) (jsonp-rpc! "/jsonp/package/author/add" `((pkg . ,name) (author . ,a))))
               added-authors)
       (andmap (lambda (a) (jsonp-rpc! "/jsonp/package/author/del" `((pkg . ,name) (author . ,a))))
               removed-authors)
       (andmap (lambda (e) (jsonp-rpc! "/jsonp/package/version/add" `((pkg . ,name)
                                                                      (version . ,(car e))
                                                                      (source . ,(cadr e)))))
               changed-versions)
       (andmap (lambda (v) (jsonp-rpc! "/jsonp/package/version/del" `((pkg . ,name)
                                                                      (version . ,v))))
               removed-versions)

       (let* ((new-pkg (or old-pkg (hash)))
              (new-pkg (hash-set new-pkg 'name name))
              (new-pkg (hash-set new-pkg 'description description))
              (new-pkg (hash-set new-pkg 'author (string-join authors)))
              (new-pkg (hash-set new-pkg 'authors authors))
              (new-pkg (hash-set new-pkg 'tags tags))
              (new-pkg (hash-set new-pkg 'versions (friendly-versions versions/default)))
              (new-pkg (hash-set new-pkg 'source source))
              (new-pkg (hash-set new-pkg 'search-terms (compute-search-terms new-pkg)))
              (new-pkg (hash-set new-pkg '_LOCALLY_MODIFIED_ #t)))
         (replace-package! old-pkg new-pkg)
         #t)))

;; Based on (and copied from) the analogous code in meta/pkg-index/official/static.rkt
(define (compute-search-terms ht)
  (let* ([st (hasheq)]
         [st (for/fold ([st st])
                       ([t (in-list (hash-ref ht 'tags (lambda () '())))])
               (hash-set st (string->symbol t) #t))]
         [st (hash-set
              st
              (string->symbol
               (format "ring:~a" (hash-ref ht 'ring (lambda () 2)))) #t)]
         [st (for/fold ([st st])
                       ([a (in-list (string-split (hash-ref ht 'author (lambda () ""))))])
               (hash-set
                st (string->symbol (format "author:~a" a)) #t))]
         [st (if (null? (hash-ref ht 'tags (lambda () '())))
                 (hash-set st ':no-tag: #t)
                 st)]
         [st (if (hash-ref ht 'checksum-error #f)
                 (hash-set st ':error: #t)
                 st)]
         [st (if (equal? "" (hash-ref ht 'description ""))
                 (hash-set st ':no-desc: #t)
                 st)]
         [st (if (null? (hash-ref ht 'conflicts (lambda () '())))
                 st
                 (hash-set st ':conflicts: #t))])
    st))

(define (friendly-versions draft-versions)
  (for/hash ((v draft-versions))
    (match-define (list version source) v)
    (values (string->symbol version)
            (hash 'checksum ""
                  'source source
                  'source_url (package-url->useful-url source)))))

;; Copied from meta/pkg-index/official/static.rkt
(define (package-url->useful-url pkg-url-str)
    (define pkg-url
      (string->url pkg-url-str))
    (match (url-scheme pkg-url)
      ["github"
       (match (url-path pkg-url)
         [(list* user repo branch path)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list* user repo (path/param "tree" '()) branch path)]))]
         [_
          pkg-url-str])]
      ["git"
       (match (url-path pkg-url)
         ;; xxx make this more robust
         [(list user repo)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list user repo (path/param "tree" '())
                        (path/param "master" '()))]))]
         [_
          pkg-url-str])]
      [_
       pkg-url-str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-page request)
  (parameterize ((bootstrap-active-navigation nav-search))
    (authentication-wrap
     #:request request
     (define-form-bindings request ([search-text q ""]
                                    [tags-input tags ""]))
     (define tags (for/list ((t (string-split tags-input)))
                    (match t
                      [(pregexp #px"!(.*)" (list _ tag)) (list (string->symbol tag) #f)]
                      [tag (list (string->symbol tag) #t)])))
     (bootstrap-response "Search Racket Package Index"
                         `(form ((class "form-horizontal")
                                 (role "form"))
                           (div ((class "form-group"))
                                (label ((class "col-sm-2 control-label")
                                        (for "q")) "Search terms")
                                (div ((class "col-sm-10"))
                                     (input ((class "form-control")
                                             (type "text")
                                             (placeholder "Enter free-form text to match here")
                                             (name "q")
                                             (value ,search-text)
                                             (id "q")))))
                           (div ((class "form-group"))
                                (label ((class "col-sm-2 control-label")
                                        (for "tags")) "Tags")
                                (div ((class "col-sm-10"))
                                     (input ((class "form-control")
                                             (type "text")
                                             (placeholder "tag1 tag2 tag3 ...")
                                             (name "tags")
                                             (value ,tags-input)
                                             (id "tags")))))
                           (div ((class "form-group"))
                                (div ((class "col-sm-offset-2 col-sm-10"))
                                     (button ((type "submit")
                                              (class "btn btn-primary"))
                                             (span ((class "glyphicon glyphicon-search")))
                                             " Search")))
                           (div ((class "search-results"))
                                ,@(maybe-splice
                                   (or (pair? tags) (not (equal? search-text "")))
                                   (let ((package-name-list (package-search search-text tags)))
                                     `(div
                                       (p ((class "package-count")) ,(format "~a packages found" (length package-name-list)))
                                       ,(package-summary-table package-name-list))))))))))