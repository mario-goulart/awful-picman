(module awful-picman ()

(import chicken scheme)
(declare (uses chicken-syntax))

;; Units
(use data-structures extras files posix setup-api srfi-13)

;; Eggs
(use awful free-gettext spiffy)

;; awful-picman modules
(use awful-picman-params
     awful-picman-utils
     awful-picman-db
     awful-picman-image
     awful-picman-conf
     awful-picman-dispatcher
     awful-picman-process-dir
     awful-picman-renderers
     awful-picman-db-migrations
     awful-picman-gc)

;; Change to the root dir (the directory which contains the
;; metadata dir
(set! root-dir
      (let ((d (find-root-dir (current-directory))))
        (when d (change-directory d))
        "."))

;; gettext stuff
(define (i18n-language)
  (or (language)
      (get-environment-variable "LANG")
      (get-environment-variable "LC_ALL")))

(define (create-thumbnails-dirs)
  (let ((thumbs-dir (make-pathname dot-dirname thumbnails-dirname)))
    (debug 1 "create-thumbnails-dirs: thumbs-dir = ~a" thumbs-dir)
    (for-each (lambda (dimension)
                (let ((dir (make-pathname thumbs-dir (->string dimension))))
                  (debug 1 "create-thumbnails-dirs: ~a" dir)
                  (create-directory dir 'with-parents)))
              (list (thumbnails/small-dimension)
                    (thumbnails/zoom-dimension)))))

(define (initialize-metadata-dir force?)
  (create-directory metadata-dir)
  (create-thumbnails-dirs)
  (parameterize ((setup-verbose-mode (verbose?))
                 (run-verbose (verbose?)))
    (for-each (lambda (asset)
                (copy-file (make-pathname assets-install-dir asset)
                           (make-pathname metadata-dir asset)
                           prefix: metadata-dir))
              '("assets" "locale")))
  (initialize-database (make-pathname metadata-dir db-filename) force?))

(define (initialize #!optional force?)
  (info "Initializing ~a ..." (current-directory))
  (initialize-metadata-dir force?)
  (process-dir "."))

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
  web server's static data and thumbnails are stored.

--init-only
  Does what --init does, but doesn't start the web server.

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

--gc
  Performs garbage collection on thumbnails and the database:
  removes all records that reference files that don't exist anymore.

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

  (and-let* ((d (cmd-line-arg '--debug args)))
    (debug-level (or (string->number d) 0)))

  (load-global-conf)
  (load-user-conf)
  (load-local-conf)

  (debug 1 "metadata-dir: ~a" metadata-dir)

  ;; Set _ for gettext
  (set! _ (let ()
            (textdomain "awful-picman")
            ((make-gettext "awful-picman"
                           (i18n-language)
                           "./.awful-picman/locale")
             'getter)))

  ;; Set the default language for the OCR in case it is unset
  (unless (ocr-default-language)
    (ocr-default-language
     (let ((lang (i18n-language)))
       (and lang
            (case (string->symbol (string-downcase lang))
              ((pt_br) 'por)
              ((de_de) 'deu)
              ((fr_fr) 'fra)
              ((it_it) 'ita)
              ((es_es) 'spa)
              (else "eng"))))))

  (db-credentials (make-pathname metadata-dir db-filename))

  (let ((init-only? (member "--init-only" args)))
    (when (or (member "--init" args) init-only?)
      (initialize (and (member "--force" args) #t)))

    (unless (directory-exists? metadata-dir)
      (fprintf (current-error-port)
               (string-append
                "Could not find a metadata directory.  "
                "Did you create it with --init?\n"))
      (exit 1))

    ;; Run migrations if necessary
    (maybe-migrate-db!)

    (when init-only?
      (exit 0)))

  (when (member "--gc" args)
    (handle-exceptions exn
      (begin
        (print-call-chain (current-error-port))
        (print-error-message exn (current-error-port))
        (exit 1))
      (begin
        (gc!)
        (exit 0))))

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

) ;; end module
