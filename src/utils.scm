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

(define (drop-web-path-prefix prefix path)
  (let ((dir (drop-path-prefix prefix path)))
        (if (or (equal? dir "/")
                (equal? dir ""))
            "."
            dir)))

(define (debug level fmt . args)
  (when (<= level (debug-level))
    (apply fprintf `(,(current-error-port) ,((debug-formatter) level fmt) ,@args))))

(define (info fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

(define (info-error fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n") args))))

(define (info* fmt . args)
  ;; Like info, but only for when (verbose?) is truthy
  (when (verbose?)
    (apply info (cons fmt args))))

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

(define (maybe-string-null->false str)
  (if (equal? str "")
      #f
      str))

(define (list-directory dir)
  ;; List directory `dir' content.  Directories first, then images,
  ;; then other files.
  (let* ((items (glob (make-pathname dir "*")))
         (dirs (sort (filter directory? items) string<))
         (pics (sort (filter image-file? items) string<))
         (other (remove (lambda (i)
                          (or (directory? i)
                              (image-file? i)))
                        items)))
    (append dirs pics other)))

(define (query-string)
  (uri-query (request-uri (current-request))))

(define (append-to-query-string vars/vals)
  (form-urlencode (append vars/vals (query-string))))

(define (path-split path)
  (string-split path "/"))

(define (format-size/bytes n)
  (define num/si ;; Stolen from fmt
    (let* ((names10 '#("" "k" "M" "G" "T" "E" "P" "Z" "Y"))
           (names2 (list->vector
                    (cons ""
                          (cons "Ki" (map (lambda (s) (string-append s "i"))
                                          (cddr (vector->list names10))))))))
      (lambda (n . o)
        (let-optionals* o ((base 1024)
                           (suffix "")
                           (names (if (= base 1024) names2 names10)))
           (let* ((k (min (inexact->exact (floor (/ (log n) (log base))))
                          (vector-length names)))
                  (n2 (/ (round (* (/ n (expt base k)) 10)) 10)))
             (conc (if (integer? n2)
                       (number->string (inexact->exact n2))
                       (exact->inexact n2))
                   (vector-ref names k)
                   (if (zero? k) "" suffix)))))))
  (string-append (num/si n) "B"))
