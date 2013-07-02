(define verbose? (make-parameter #f))
(define pics-web-dir (make-parameter "/pics"))
(define thumbnails-web-dir (make-parameter "/thumbnails"))

(define dot-dirname ".awful-view")
(define thumbnails-dirname "thumbnails")
(define db-filename "awful-view.db")

(define assets-install-dir
  (make-pathname (list (installation-prefix)
                       "lib"
                       "chicken"
                       (number->string (##sys#fudge 42)))
                 "awful-view"))

(define root-dir (or (find-root-dir (current-directory))
                     "."))

(define metadata-dir
  (if root-dir
      (make-pathname root-dir dot-dirname)
      (make-pathname "." dot-dirname)))

(define global-conf-dir
  (make-pathname (get-environment-variable "HOME") dot-dirname))

(define (create-thumbnails-dirs path)
  ;; path is relative to metadata-dir
  (let ((thumbs-dir (make-pathname (list dot-dirname path)
                                   thumbnails-dirname)))
    (debug "create-thumbnails-dirs: path = ~a" path)
    (debug "create-thumbnails-dirs: thumbs-dir = ~a" thumbs-dir)
    (for-each (lambda (dimension)
                (let ((dir (make-pathname thumbs-dir (->string dimension))))
                  (debug "create-thumbnails-dir: ~a" dir)
                  (create-directory dir 'with-parents)))
              (thumbnails/max-dimensions))))

(define (initialize-metadata-dir force?)
  (create-directory metadata-dir)
  (create-thumbnails-dirs ".")
  (parameterize ((setup-verbose-mode (verbose?))
                 (run-verbose (verbose?)))
    (for-each (lambda (asset)
                (copy-file (make-pathname assets-install-dir asset)
                           (make-pathname metadata-dir asset)
                           prefix: metadata-dir))
              '("js" "css" "img")))
  (initialize-database (make-pathname metadata-dir db-filename) force?))

(define (initialize #!optional recursive? force?)
  (info "Initializing ~a ..." (current-directory))
  (unless (memq (thumbnails/zoom-dimension)
                (thumbnails/max-dimensions))
    (thumbnails/max-dimensions
     (append (thumbnails/max-dimensions)
             (list (thumbnails/zoom-dimension)))))
  (initialize-metadata-dir force?)
  (process-dir "." recursive?))

(define (process-dir dir recursive?)
  (debug "Processing ~a" dir)
  (let ((image-files (filter image-file? (glob (make-pathname dir "*")))))
    (debug "Image files: ~S" image-files)
    ;; Generate thumbnails for images in the current directory
    (for-each (lambda (image-file)
                (for-each (lambda (dimension)
                            (image->thumbnail image-file dimension)
                            (db/maybe-insert-file! (drop-path-prefix root-dir image-file)))
                          (thumbnails/max-dimensions)))
              image-files)
    (when recursive?
      ;; Recur into subdirectories
      (let ((dirs (filter directory? (glob (make-pathname dir "*")))))
        (debug "  directories: ~S" dirs)
        (for-each (lambda (subdir)
                    (process-dir subdir #t))
                  dirs)))))

(define (awful-view)

  (enable-sxml #t)
  (enable-db)
  
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

(define (usage #!optional exit-code)
  (print "usage")
  (when exit-code
     (exit exit-code)))

(let ((args (command-line-arguments)))

  (when (or (member "--help" args)
            (member "-help" args)
            (member "-h" args))
     (usage 0))

  (when (member "--verbose" args)
    (verbose? #t))

  (debug "metadata-dir: ~a" metadata-dir)

  (db-credentials (make-pathname metadata-dir db-filename))
  
  (when (member "--init" args)
    (initialize (and (member "--recursive" args) #t)
                (and (member "--force" args) #t)))

  (unless (directory-exists? metadata-dir)
    (fprintf (current-error-port)
             (string-append
              "Could not find a metadata directory.  "
              "Did you create it with --init?\n"))
    (exit 1))
  
  (let ((dev-mode? (and (member "--development-mode" args) #t))
        (port (cmd-line-arg "--port" args)))
    
  (awful-start
   (lambda ()
     (load-apps '()) ;; to force development actions when
                     ;; --development-mode is given on the command
                     ;; line (maybe a bug in awful?)
     (awful-view))
   port: (and port (string->number port))
   dev-mode?: dev-mode?)
  ))
