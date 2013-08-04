(define (awful-picman)

  (enable-sxml #t)
  (enable-db)
  (literal-script/style? #t)

  (page-access-control
   (lambda (dummy)
     (equal? "localhost"
             (ip->hostname
              (list->u8vector
               (map string->number
                    (string-split (remote-address) ".")))))))

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
                                      "/js/awful-picman.js"))
      css: '("/css/bootstrap.min.css"
             "/css/awful-picman.css")))
  
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
          (send-static-file (pathname-strip-directory thumbnail))))))

  ;;;
  ;;; Assets
  ;;;
  (define (assets-matcher req-path)
    (and (or (string-prefix? "/css/" req-path)
             (string-prefix? "/js/" req-path)
             (string-prefix? "/img/" req-path))
         (list req-path)))

  (define-page assets-matcher
    (lambda (file)
      (debug 2 "assets handler: handling ~a" file)
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
             (json-write (map db-album-title (db-albums)))))))
    no-template: #t)

  ;;;
  ;;; Albums
  ;;;
  (define-pics-page (irregex (string-append (albums-web-dir) "(/.*)*"))
    (lambda (path)
      (debug 1 "albums handler: handling ~a" path)
      (let ((album (drop-web-path-prefix (albums-web-dir) path)))
        (render-pics (if (equal? album ".")
                         #f ;; albums index
                         album)
                     'album
                     (or ($ 'page as-number) 0)))))

  ;;;
  ;;; Folders
  ;;;
  (define-pics-page (irregex (string-append (folders-web-dir) "(/.*)*"))
    (lambda (path)
      (debug 1 "folders handler: handling ~a" path)
      (let ((dir (drop-web-path-prefix (folders-web-dir) path)))
        (if (file-exists? dir)
            (begin
              (process-dir dir #f)
              (render-pics dir 'folder (or ($ 'page as-number) 0)))
            (lambda ()
              (send-status 404 "Not found"))))))

  ;;
  ;; /
  ;;
  (define-page (main-page-path)
    (lambda ()
      (redirect-to (folders-web-dir))))
  )
