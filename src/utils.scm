(define (find-root-dir dir)
  ;; Find the root dir, looking back in the filesystem hierarchy,
  ;; starting from `dir'.
  (let ((dot-dir (make-pathname dir dot-dirname)))
    (if (and (file-exists? dot-dir)
             (directory? dot-dir))
        dir
        (if (equal? dir (pathname-directory dir)) ;; system root dir
            #f
            (find-root-dir (pathname-directory dir))))))

(define (default-thumbnail-dimension)
  (car (thumbnails/max-dimensions)))

(define (drop-path-prefix prefix path)
  (let ((p (substring path (string-length prefix))))
    (cond ((equal? p "")
           "/")
          ((equal? (substring p 0 1) "/")
           (substring p 1))
          (else p))))

(define (debug fmt . args)
  (when (verbose?)
    (apply fprintf `(,(current-error-port) ,(string-append fmt "\n") ,@args))))
    
(define (info fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))
