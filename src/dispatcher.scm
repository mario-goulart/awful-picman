(define (awful-view)

  (enable-sxml #t)
  (enable-db)

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
    (debug "assets-matcher: ~a" req-path)
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
  ;;; Directories & other stuff
  ;;;
  (define-page (irregex (string-append (pics-web-dir) "(/.*)*"))
    (lambda (path)      
      (let ((dir (drop-path-prefix (pics-web-dir) path)))
        (when (or (equal? dir "/")
                  (equal? dir ""))
          (set! dir "."))
        (debug "regex handler: dir: ~a" dir)
        (if (file-exists? dir)
            (begin
              (process-dir dir #f)
              (render-directory-content dir))
            (lambda ()
              (send-status 404 "Not found")))))
    charset: "utf-8"
    doctype: "<!DOCTYPE html>"
    use-ajax: "/js/jquery.min.js"
    headers: `(,(include-javascript "/js/bootstrap.min.js"))
    css: '("/css/bootstrap.min.css"
           "/css/awful-view.css"))
  
  (define-page (main-page-path)
    (lambda ()
      (redirect-to (pics-web-dir))))
  )
