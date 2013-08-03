(define (render-breadcrumbs path root-label web-root-dir)

  (define (home link?)
    (if link?
        `(li (a (@ (href ,web-root-dir)) ,root-label))
        root-label))

  (define divider '(li (span (@ (class "divider")) ">")))

  (define (make-path parts)
    (string-intersperse (cons web-root-dir parts) "/"))

  (define (link-breadcrumb parts #!key with-divider?)
    `(li (a (@ (href ,(make-path parts))) ,(last parts))
         ,(if with-divider?
              divider
              '())))

  `(ul (@ (class "breadcrumb"))
       ,(if (or (equal? path ".") ;; Is this necessary?
                (equal? path "/"))
            (home #f)
            (let ((parts (string-split path "/")))
              (let loop ((parts parts)
                         (bc '()))
                (if (null? parts)
                    (intersperse (cons (home #t) bc) divider)
                    (loop (butlast parts)
                          (cons (link-breadcrumb parts) bc))))))))
