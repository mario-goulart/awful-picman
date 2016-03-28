(define (render-breadcrumbs path root-label web-root-dir)
  ;; `path' can be either a string (representing a path) or a pair
  ;; (<album-id> . <album-title>)

  (define (home link?)
    (if link?
        `(li (a (@ (href ,web-root-dir)) ,root-label))
        root-label))

  (define (make-path parts)
    (string-intersperse (cons web-root-dir parts) "/"))

  (define (link-breadcrumb parts)
    `(li (a (@ (href ,(make-path parts))) ,(last parts))))

  `(ol (@ (class "breadcrumb"))
       ,(if (or (equal? path ".") ;; Is this necessary?
                (equal? path "/"))
            (home #f)
            (if (pair? path)
                ;; Albums cannot be nested, and we need to link album
                ;; id's, so we encode id/title into a pair for albums.
                (list (home #t)
                      '()
                     `(li (a (@ (href ,(car path))) ,(cdr path))))
                ;; Handle normal paths
                (let ((parts (string-split path "/")))
                  (let loop ((parts parts)
                             (bc '()))
                    (if (null? parts)
                        (cons (home #t) bc)
                        (loop (butlast parts)
                              (cons (link-breadcrumb parts) bc)))))))))
