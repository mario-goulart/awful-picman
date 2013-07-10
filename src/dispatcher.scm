(define (awful-view)

  (enable-sxml #t)
  (enable-db)
  (literal-script/style? #t)

  ;;;
  ;;; Page definer
  ;;;
  (define (define-pics-page matcher handler)
    (define-page matcher
      (case-lambda
        (() handler)
        ((path) (handler path))
        (args (apply handler args)))
      charset: "utf-8"
      doctype: "<!DOCTYPE html>"
      use-ajax: "/js/jquery.min.js"
      headers: `(,(include-javascript "/js/bootstrap.min.js"
                                      "/js/awful-view.js"))
      css: '("/css/bootstrap.min.css"
             "/css/awful-view.css")))
  
  ;;;
  ;;; Thumbnails
  ;;;
  (define (thumbnail-matcher req-path)
    (debug "req-path: ~a" req-path)
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
      (lambda ()
        (parameterize ((root-path (pathname-directory thumbnail)))
          (send-static-file (pathname-strip-directory thumbnail))))))

  ;;;
  ;;; Assets
  ;;;
  (define (assets-matcher req-path)
    (and (or (string-prefix? "/css" req-path)
             (string-prefix? "/js" req-path)
             (string-prefix? "/img" req-path))
         (list req-path)))

  (define-page assets-matcher
    (lambda (file)
      (lambda ()
        (parameterize ((root-path (make-pathname metadata-dir
                                                 (pathname-directory file))))
          (send-static-file (pathname-strip-directory file))))))

  ;;;
  ;;; db
  ;;;
  (define-page "/db/tags"
    (lambda ()
      (awful-response-headers '((content-type "application/json")))
      `(literal
        ,(with-output-to-string
           (lambda ()
             (json-write (db-tags))))))
    no-template: #t)

  (define-page "/db/albums"
    (lambda ()
      (awful-response-headers '((content-type "application/json")))
      `(literal
        ,(with-output-to-string
           (lambda ()
             (json-write (db-albums))))))
    no-template: #t)

  ;;;
  ;;; Albums
  ;;;
  (define (albums-matcher req-path)
    (let ((parts (string-split req-path "/")))
      (match parts
        (("albums") (list #f)) ;; list albums
        (("albums" album) (list album)) ;; render album
        (else #f))))

  (define-pics-page albums-matcher
    (lambda (album)
      (debug "albums: ~a" album)
      (render-pics album render-album-content)))

  ;;;
  ;;; Directories & other stuff
  ;;;
  (define-pics-page (irregex (string-append (pics-web-dir) "(/.*)*"))
    (lambda (path)
      (let ((dir (drop-path-prefix (pics-web-dir) path)))
        (when (or (equal? dir "/")
                  (equal? dir ""))
          (set! dir "."))
        (if (file-exists? dir)
            (begin
              (process-dir dir #f)
              (render-pics dir render-dir-content))
            (lambda ()
              (send-status 404 "Not found"))))))

  ;;
  ;; /
  ;;
  (define-page (main-page-path)
    (lambda ()
      (redirect-to (pics-web-dir))))
  )
