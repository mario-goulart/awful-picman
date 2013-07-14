(define dot-dirname ".awful-view")
(define thumbnails-dirname "thumbnails")
(define db-filename "awful-view.db")

;; Where chicken-install will install static files served by the web
;; server.  This stuff will be copied to the metadata dir on --init.
(define assets-install-dir
  (make-pathname (list (installation-prefix)
                       "lib"
                       "chicken"
                       (number->string (##sys#fudge 42)))
                 "awful-view"))

;; Change to the root dir (the directory which contains the
;; metadata dir
(define root-dir
  (let ((d (find-root-dir (current-directory))))
    (when d (change-directory d))
    "."))

(define metadata-dir
  (make-pathname "." dot-dirname))

(define global-conf-dir
  (make-pathname (get-environment-variable "HOME") dot-dirname))

;; gettext stuff
(define (i18n-language)
  (or (language)
      (get-environment-variable "LANG")
      (get-environment-variable "LC_ALL")))

(define _)

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
              '("js" "css" "img" "locale")))
  (initialize-database (make-pathname metadata-dir db-filename) force?))

(define (initialize #!optional recursive? force?)
  (info "Initializing ~a ..." (current-directory))
  (initialize-metadata-dir force?)
  (process-dir "." recursive?))

(define (process-dir dir recursive?)
  (debug "Processing ~a" dir)
  (let ((image-files (filter image-file? (glob (make-pathname dir "*"))))
        (db-dir-filenames (db-dir-pics dir)))
    (debug "Image files: ~S" image-files)
    ;; Generate thumbnails for images in the current directory
    (for-each (lambda (image-file)
                (for-each (lambda (dimension)
                            (image->thumbnail image-file dimension)
                            (unless (member (pathname-strip-directory image-file)
                                            db-dir-filenames)
                              (insert/update-pic! (if (equal? dir root-dir)
                                                      (drop-path-prefix root-dir image-file)
                                                      image-file))))
                          (thumbnails/max-dimensions)))
              image-files)
    (when recursive?
      ;; Recur into subdirectories
      (let ((dirs (filter directory? (glob (make-pathname dir "*")))))
        (debug "  directories: ~S" dirs)
        (for-each (lambda (subdir)
                    (process-dir subdir #t))
                  dirs)))))


(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #this [ <options> ]

<options>:

--init
  Initialize the current directory to be used in subsequent runs of
  this program.  It creates a directory (#dot-dirname) where metadata,
  web server's static data and thumbnails are stored.  During the init
  step, #this creates thumbnails for all images in the current directory.
  If --recursive (see below) is not provided, it will not recur into
  directories.

--recursive
  To be used with --init.  Indicates that #this is to recur into
  directories.

--development-mode
  Put awful (the server) in development mode.

--port=<port number>
  Port for the web server to use.

--force
  --init will not overwrite the database file if it exists, unless this
  option is provided.  In other words, --force makes #this overwrite
  the database file when called with --init.

EOF
             port)
    (when exit-code
      (exit exit-code))))

(let ((args (command-line-arguments)))

  (when (or (member "--help" args)
            (member "-help" args)
            (member "-h" args))
     (usage 0))

  (when (member "--verbose" args)
    (verbose? #t))

  (debug "metadata-dir: ~a" metadata-dir)

  ;; Set _ for gettext
  (set! _ (let ()
            (textdomain "awful-view")
            ((make-gettext "awful-view"
                           (i18n-language)
                           "./.awful-view/locale")
             'getter)))

  (db-credentials (make-pathname metadata-dir db-filename))

  ;; Add (thumbnails/max-dimensions) to the list of thumbnail
  ;; dimensions (before initialize is called).
  (unless (memq (thumbnails/zoom-dimension)
                (thumbnails/max-dimensions))
    (thumbnails/max-dimensions
     (append (thumbnails/max-dimensions)
             (list (thumbnails/zoom-dimension)))))

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
     dev-mode?: dev-mode?)))
