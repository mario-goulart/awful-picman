(module awful-picman-dispatcher (awful-picman)

(import chicken scheme)
(use data-structures extras files irregex ports posix srfi-1 srfi-13)
(use intarweb json spiffy awful awful-sql-de-lite matchable)
(use awful-picman-params
     awful-picman-utils
     awful-picman-image
     awful-picman-db
     awful-picman-process-dir
     awful-picman-renderers)

(define (awful-picman)

  (enable-sxml #t)
  (enable-db)
  (literal-script/style? #t)

  (define (ajax-spinner)
    ;; Only show the spinner after 400ms
    (add-javascript "
var $loader = $('#ajax-busy')
var timer;

$(document)
    .ajaxStart(function()
    {
        timer && clearTimeout(timer);
        timer = setTimeout(function()
        {
            $loader.show();
        },
        400);
    })
    .ajaxStop(function()
    {
        clearTimeout(timer);
        $loader.hide();
    });
")
    `(div (@ (id "ajax-busy"))))

  ;;;
  ;;; Page definer
  ;;;
  (define (define-pics-page matcher handler)
    (define-page matcher
      (case-lambda
        (() (handler))
        ((path) (handler path))
        (args (apply handler args)))
      title: "awful-picman"
      charset: "utf-8"
      doctype: "<!DOCTYPE HTML>" ;; this is important for $(window).height() to report proper values
      use-ajax: "/assets/jquery/js/jquery.min.js"
      css: '("/assets/awful-picman/css/reset.css"
             "/assets/bootstrap/css/bootstrap.min.css"
             "/assets/autocomplete/css/autocomplete.css"
             "/assets/awful-picman/css/awful-picman.css")))


  (define (define-data matcher data)
    (define-page matcher
      (case-lambda
        (args (apply (lambda args
                       (awful-response-headers '((content-type text/plain)))
                       `(literal
                         ,(with-output-to-string
                            (lambda ()
                              (write (apply data args))))))
                     args)))
      no-template: #t))

  ;;;
  ;;; Configuration for the client-side
  ;;;
  (define-data "/conf" ;; FIXME: make a parameter
    (lambda ()
      `((thumbnails/max-dimension . 300)
        (thumbnails/zoom-dimension . 1700)
        (i18n-language . pt-br)))) ;; FIXME


  (define (read-from-request) ;; FIXME: move to somewhere else (utils?)
    (let* ((req (current-request))
           (headers (request-headers req))
           (content-length (header-value 'content-length headers)))
      (cond ((and content-length (zero? content-length))
             "")
            ((not content-length)
             (error 'echo-service "Set content-length, sloppy client"))
            (else
             (with-input-from-string
                 (read-string content-length
                              (request-port req))
               read)))))

  ;;;
  ;;; Pic info
  ;;;
  (define-data (irregex "/read-pic-info/[0-9]+")
    (lambda (path)
      (let* ((pic-id (string->number (pathname-strip-directory path)))
             (pic-info (db-get-pic-by-id pic-id)))
        (db-pic->alist pic-info))))

  (define-page (irregex "/write-pic-info/[0-9]+")
    (lambda (path)
      (let* ((pic-id (string->number (pathname-strip-directory path)))
             (data (read-from-request))
             (date (alist-ref 'date data)))
        (insert/update-pic! pic-id
                            descr: (alist-ref 'description data)
                            decade: (and date (car date))
                            year: (and date (cadr date))
                            month: (and date (caddr date))
                            day: (and date (cadddr date))
                            tags: (alist-ref 'tags data)
                            albums: (alist-ref 'albums data))
        ""))
    no-template: #t
    method: 'post)


  ;;;
  ;;; Album info
  ;;;
  (define-data "/albums-info"
    db-albums->alist)

  (define-data (irregex "/read-album-info/[0-9]+")
    (lambda (path)
      (let* ((album-id (string->number (pathname-strip-directory path)))
             (album-info (db-get-album-by-id album-id)))
        (db-album->alist album-info))))

  (define-page (irregex "/write-album-info/[0-9]+")
    (lambda (path)
      (let ((album-id (string->number (pathname-strip-directory path)))
            (data (read-from-request)))
        (db-update-album! (alist-ref 'id data)
                          (alist-ref 'title data)
                          (alist-ref 'description data))
        ""))
    no-template: #t
    method: 'post)

  ;;;
  ;;; Thumbnails
  ;;;
  (define (thumbnail-matcher req-path)
    (and (string-prefix? (make-absolute-pathname #f thumbnails-dirname)
                         req-path)
         (image-file? req-path)
         (let ((thumbnail
                (make-pathname metadata-dir
                               (maybe-replace-thumbnail-extension req-path))))
           (and (file-exists? thumbnail)
                (list req-path thumbnail)))))

  (define-page thumbnail-matcher
    (lambda (req-path thumbnail)
      (debug 1 "thumbnails handler: handling ~a" req-path)
      (lambda ()
        (parameterize ((root-path (pathname-directory thumbnail)))
          (send-static-file (pathname-strip-directory thumbnail)))))
    no-db: #t)

  ;;;
  ;;; Assets
  ;;;
  (define (assets-matcher req-path)
    (and (string-prefix? "/assets/" req-path)
         (list req-path)))

  (define-page assets-matcher
    (lambda (file)
      (debug 2 "assets handler: handling ~a" file)
      (lambda ()
        (let ((file-full-path (make-pathname (list (root-path) metadata-dir)
                                             file)))
          (parameterize ((root-path (make-pathname metadata-dir
                                                   (pathname-directory file))))
            (if (file-read-access? file-full-path)
                (send-static-file (pathname-strip-directory file))
                (send-status
                 'not-found
                 "<p>The resource you requested could not be found</p>"))))))
    no-template: #t
    no-db: #t)

  ;;;
  ;;; db
  ;;;
  (define (write-suggestions suggestions)
    (lambda ()
      (awful-response-headers '((content-type "application/json")))
      (with-request-variables (query)
        `(literal
          ,(with-output-to-string
             (lambda ()
               (json-write `#(("suggestions" ,@(suggestions query))))))))))

  (define-page "/db/tags"
    (write-suggestions
     (lambda (query)
       (filter (lambda (tag)
                 (substring-index-ci query tag))
             (db-tags))))
    no-template: #t)

  (define-page "/db/albums"
    (write-suggestions
     (lambda (query)
       (filter (lambda (album)
                 (substring-index-ci query album))
               (map db-album-title (db-albums)))))
    no-template: #t)

  ;;;
  ;;; Albums
  ;;;
  (define-pics-page (irregex (string-append (albums-web-dir) "(/.*)*"))
    (lambda (path)
      (debug 1 "albums handler: handling ~a" path)
      (let* ((album (drop-web-path-prefix (albums-web-dir) path))
             (album-index? (equal? album ".")))
        (list (ajax-spinner)
              (render-navbar 'albums)
              (render-pics (if album-index?
                               #f ;; albums index
                               album)
                           'album)
              (include-javascript
               "/assets/bootstrap/js/bootstrap.min.js"
               "/assets/spock/js/spock-runtime-debug.js" ;; FIXME: when debug, spock-runtime-debug.js
               "/assets/load-image/js/load-image.all.min.js"
               "/assets/autocomplete/js/jquery.autocomplete.min.js"
               (if album-index?
                   "/assets/awful-picman/js/awful-picman-albums.js"
                   "/assets/awful-picman/js/awful-picman-pics.js")) ;;; FIXME: move to define-pics-page
              ))))

  ;;;
  ;;; Folders
  ;;;
  (define-pics-page (irregex (string-append (folders-web-dir) "(/.*)*"))
    (lambda (path)
      (with-request-variables (done)
        (debug 1 "folders handler: handling ~a" path)
        (let ((dir (drop-web-path-prefix (folders-web-dir) path)))
          (if (file-exists? dir)
              (let ((missing-thumbnails (find-missing-thumbnails dir)))
                (if (or done (null? missing-thumbnails))
                    (list (ajax-spinner)
                          (render-navbar 'folders)
                          (render-pics dir 'folder)
                          (include-javascript
                           "/assets/bootstrap/js/bootstrap.min.js"
                           "/assets/spock/js/spock-runtime-debug.js" ;; FIXME: when debug, spock-runtime-debug.js
                           "/assets/load-image/js/load-image.all.min.js"
                           "/assets/autocomplete/js/jquery.autocomplete.min.js"
                           "/assets/awful-picman/js/awful-picman-pics.js")) ;;; FIXME: move to define-pics-page
                    (poll-thumbnails-conversion dir path missing-thumbnails)))
              (lambda ()
                (send-status 404 "Not found")))))))

  ;;;
  ;;; Tags
  ;;;
  (define-pics-page (tags-web-dir)
    (lambda ()
      (debug 1 "tags handler")
      (list (ajax-spinner)
            (render-tags))))

  ;;;
  ;;; Filters
  ;;;
  (define-pics-page (irregex (string-append (filters-web-dir) "(/.*)*"))
    (lambda (path)
      (let ((page (or ($ 'page as-number) 0)))
        (debug 1 "filters handler: ~S" path)
        (list
         (ajax-spinner)
         (match (cdr (path-split path))
                (("by-tags")
                 (with-request-variables ((include-tags as-list)
                                          (exclude-tags as-list))
                   (let ((include-tags (if include-tags
                                           (delete "" (map string-trim-both include-tags) equal?)
                                           '()))
                         (exclude-tags (if exclude-tags
                                           (delete "" (map string-trim-both exclude-tags) equal?)
                                           '())))
                     (debug 1 "include-tags: ~S" include-tags)
                     (render-pics (cons include-tags exclude-tags) 'filter/by-tags page))))

                (("without-album")
                 (render-pics #f 'filter/without-album page))

                (("without-tag")
                 (render-pics #f 'filter/without-tag page))

                (else (render-filters)))))))
  ;;
  ;; /
  ;;
  (define-page (main-page-path)
    (lambda ()
      (redirect-to (albums-web-dir))))
  )

) ;; end module
