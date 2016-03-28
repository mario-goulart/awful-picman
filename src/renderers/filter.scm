(define (render-filter-matches matches)
  (let ((num-matches (length matches)))
    `(h4 ,(if (zero? num-matches)
              (_ "No match")
              `(,num-matches " " ,(if (= 1 num-matches)
                                      (_ "match")
                                      (_ "matches")))))))

(define (render-filters)
  (define (filter-link path text)
    `(li (a (@ (href ,(make-pathname (filters-web-dir) path))) ,text)))
  `(,(render-navbar #f)
    (div (@ (id "content")) ;; FIXME: move to a more generic place
         (ul
          ,(filter-link "by-tags" (_ "Filter by tags"))
          ,(filter-link "without-album" (_ "Pics without album"))
          ,(filter-link "without-tag" (_ "Pics without tag"))))))

(define (render-filter/by-tags include-tags exclude-tags)
  `((div (@ (id "filter-tags")
            (data-include-tags
             ,(string-intersperse include-tags "\t"))
            (data-exclude-tags
             ,(string-intersperse exclude-tags "\t"))))
    (div (@ (id "filter-input-container")))
    ,(if (null? include-tags)
         '()
         (let ((filtered-pic-paths (db-tag-filter include-tags exclude-tags)))
           (debug 2 "render-filtered-pictures: filter results: ~S" filtered-pic-paths)
           `((div (@ (id "filter-matches"))
                  ,(render-filter-matches filtered-pic-paths))
             ,(render-thumbnails filtered-pic-paths))))))
