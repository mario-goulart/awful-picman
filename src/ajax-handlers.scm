(define (json-array->list-wo-nulls json-array)
  (remove (lambda (item)
            (equal? item ""))
          (with-input-from-string json-array json-read)))

(define (update-pic-info!)
  (with-request-variables (id
                           path
                           (descr (nonempty as-string))
                           (tags (nonempty as-string))
                           (albums (nonempty as-string))
                           (decade (nonempty as-string))
                           (year (nonempty as-string))
                           (month (nonempty as-string))
                           (day (nonempty as-string)))
    (let ((tags (json-array->list-wo-nulls tags))
          (albums (json-array->list-wo-nulls albums)))
      (debug "update-pic-info!: Form data:")
      (debug "  path=~a" path)
      (debug "  date=~a ~a ~a ~a" decade year month day)
      (debug "  descr=~S" descr)
      (debug "  tags=~S" tags)
      (debug "  albums=~S" albums)
      (insert/update-pic! path descr: descr
                               tags: tags
                               albums: albums
                               decade: decade
                               year: year
                               month: month
                               day: day)
      (render-modal-pic-form/ro (get-pic-from-db path) id))))

(define (update-album-info!)
  (with-request-variables (album-id
                           (descr (nonempty as-string))
                           (remove? (nonempty as-boolean)))
    (if remove?
        (db-remove-album! album-id)
        (db-update-album! album-id descr))))
