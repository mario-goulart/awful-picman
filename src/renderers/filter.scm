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

(define (render-filter/by-tags filtered-pic-paths include-tags exclude-tags pagenum)
  `((div (@ (id "filter-tags")
            (data-include-tags
             ,(string-intersperse include-tags "\t"))
            (data-exclude-tags
             ,(string-intersperse exclude-tags "\t"))))
    (div (@ (id "filter-input-container")))
    ,(if (null? include-tags)
         '()
         (begin
           (debug 2 "render-filtered-pictures: filter results: ~S" filtered-pic-paths)
           `((div (@ (id "filter-matches"))
                  ,(render-filter-matches filtered-pic-paths))
             ,(render-thumbnails filtered-pic-paths pagenum))))))

(define (render-filter/by-date items start end pagenum)
  (let* ((decade-range
          (iota (+ 1 (/ (- (current-decade) (start-decade)) 10))
                (start-decade)
                10))
         (year-range (iota 10))
         (month-range (iota 12 1))
         (day-range (iota 31 1)))
    `((form (@ (method "get")
               (action ,(make-pathname (filters-web-dir) "by-date")))
            ((p ,(_ "Start date") ": "
                ,(combo-box "start-decade" decade-range
                            default: (and start (date-decade start)))
                " "
                ,(combo-box "start-year" year-range
                            default: (and start (date-year start)))
                " "
                ,(combo-box "start-month" month-range
                            default: (and start (date-month start)))
                " "
                ,(combo-box "start-day" day-range
                            default: (and start (date-day start)))
                " (" ,(_"Decade") "/" ,(_ "Year") "/" ,(_ "Month") "/" ,(_ "Day") ")")
             (p ,(_ "End date") ": "
                ,(combo-box "end-decade" decade-range
                            default: (if end
                                         (date-decade end)
                                         (current-decade)))
                " "
                ,(combo-box "end-year" year-range
                            default: (if end
                                         (date-year end)
                                         (- (current-year) (current-decade))))
                " "
                ,(combo-box "end-month" month-range
                            default: (if end
                                         (date-month end)
                                         (current-month)))
                " "
                ,(combo-box "end-day" day-range
                            default: (if end
                                         (date-day end)
                                         (current-day)))
                " (" ,(_"Decade") "/" ,(_ "Year") "/" ,(_ "Month") "/" ,(_ "Day") ")")
             (p (input (@ (type "submit"))))))
      ,(if (and end (not (date-decade end)))
           '()
           (render-filter-matches items))
      ,(render-thumbnails items pagenum))))
