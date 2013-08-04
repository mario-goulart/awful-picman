(define dot-dirname ".awful-picman")
(define thumbnails-dirname "thumbnails")
(define db-filename "awful-picman.db")

;; Where chicken-install will install static files served by the web
;; server.  This stuff will be copied to the metadata dir on --init.
(define assets-install-dir
  (make-pathname (list (installation-prefix)
                       "lib"
                       "chicken"
                       (number->string (##sys#fudge 42)))
                 "awful-picman"))

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
    (debug 1 "create-thumbnails-dirs: path = ~a" path)
    (debug 1 "create-thumbnails-dirs: thumbs-dir = ~a" thumbs-dir)
    (for-each (lambda (dimension)
                (let ((dir (make-pathname thumbs-dir (->string dimension))))
                  (debug 1 "create-thumbnails-dir: ~a" dir)
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
  (debug 1 "Processing ~a" dir)
  (let ((image-files (filter image-file? (glob (make-pathname dir "*"))))
        (db-dir-filenames (db-dir-pics dir)))
    (debug 1 "Image files: ~S" image-files)
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
        (debug 1 "  directories: ~S" dirs)
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

--verbose
  Print some user-oriented messages.

--debug=<debug level>
  Print debug-oriented messages.  <debug level> is a number.
  Higher numbers produce more verbose output.

EOF
             port)
    (when exit-code
      (exit exit-code))))

(let ((args (command-line-arguments)))

  (when (or (member "--help" args)
            (member "-help" args)
            (member "-h" args))
     (usage 0))

  (load-global-conf)
  (load-local-conf)

  (when (member "--verbose" args)
    (verbose? #t))

  (and-let* ((d (cmd-line-arg '--debug args)))
    (debug-level (or (string->number d) 0)))

  (debug 1 "metadata-dir: ~a" metadata-dir)

  ;; Set _ for gettext
  (set! _ (let ()
            (textdomain "awful-picman")
            ((make-gettext "awful-picman"
                           (i18n-language)
                           "./.awful-picman/locale")
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

    (info "Starting the server on port ~a." (or port (server-port)))
    (awful-start
     (lambda ()
       (load-apps '()) ;; to force development actions when
                       ;; --development-mode is given on the command
                       ;; line (maybe a bug in awful?)
       (awful-picman))
     port: (and port (string->number port))
     dev-mode?: dev-mode?)))
