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

(define-record dir-stat num-dirs num-pics num-files)

(define (get-dir-stat dir)
  (let ((content (glob (make-pathname dir "*"))))
    (make-dir-stat (length (filter directory? content))
                   (length (filter image-file? content))
                   (length (remove directory? content)))))

(define (combo-box id options #!key (first-empty? #t) default class)
  `(select (@ (id ,id)
              (name ,id)
              ,(if class
                   `(class ,class)
                   '()))
           ,@(map (lambda (opt)
                    (let ((val (if (pair? opt)
                                   (car opt)
                                   opt))
                          (text (cond ((list? opt)
                                       (cadr opt))
                                      ((pair? opt)
                                       (cdr opt))
                                      (else opt))))
                      `(option (@ (value ,val)
                                  ,(if (and default (equal? val default))
                                       '(selected)
                                       '()))
                               ,text)))
                  (if first-empty?
                      (cons "" options)
                      options))))

(define (current-year)
  (+ 1900 (vector-ref (seconds->local-time) 5)))

(define (current-decade)
  (- (current-year) (modulo (current-year) 10)))
