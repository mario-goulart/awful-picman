(define (json-array->list-wo-nulls json-array)
  (remove (lambda (item)
            (equal? item ""))
          (with-input-from-string json-array json-read)))

(define (update-pic-info!)
  (with-request-variables (id
                           path
                           descr
                           (tags (nonempty as-string))
                           (albums (nonempty as-string))
                           (decade (nonempty as-string))
                           (year (nonempty as-string))
                           (month (nonempty as-string))
                           (day (nonempty as-string)))
    (let ((tags (json-array->list-wo-nulls tags))
          (albums (json-array->list-wo-nulls albums)))
      (debug 2 "update-pic-info!: Form data:")
      (debug 2 "    path=~a" path)
      (debug 2 "    date=~a ~a ~a ~a" decade year month day)
      (debug 2 "    descr=~S" descr)
      (debug 2 "    tags=~S" tags)
      (debug 2 "    albums=~S" albums)
      (insert/update-pic! path
                          descr: descr
                          tags: tags
                          albums: albums
                          decade: decade
                          year: year
                          month: month
                          day: day)
      (render-modal-pic-form/ro (get-pic-from-db path) id))))

(define (update-album-info!)
  (with-request-variables (album-id
                           title
                           descr
                           (remove? (nonempty as-boolean)))
    (if remove?
        (db-remove-album! album-id)
        (db-update-album! album-id title descr))))

(define (batch-update-pic-info!)
  (with-request-variables (pics template-data)
    (assert pics)
    (assert template-data)
    (and-let* ((pics (chop (with-input-from-string pics json-read) 2))
               (template-data (vector->list
                               (with-input-from-string template-data json-read)))
               (tags (json-array->list-wo-nulls
                      (alist-ref "tags" template-data equal?)))
               (albums (json-array->list-wo-nulls
                        (alist-ref "albums" template-data equal?))))
      (debug 2 "batch-update-pic-info!:")
      (debug 2 "    pics: ~S" pics)
      (debug 2 "    template-data: ~S" template-data)
      (debug 2 "    tags: ~S" tags)
      (debug 2 "    albums: ~S" albums)
      (for-each
       (lambda (pic-id/path)
         ;; pic-path is actually the thumbnail path, so it
         ;; contains (thumbnails-web-dir) and the thumbnail
         ;; dimension, which have to be removed.
         (let* ((pic-id (car pic-id/path))
                (pic-path (cadr pic-id/path))
                (path-parts
                 (string-split
                  (drop-path-prefix (thumbnails-web-dir) pic-path)
                  "/"))
                (path (string-intersperse (cdr path-parts) "/"))
                (get-field (lambda (i)
                             (alist-ref i template-data equal?))))
           ;; FIXME: a single insert would be enough
           (insert/update-pic! path
                               descr: (get-field "descr")
                               tags: tags
                               albums: albums
                               decade: (get-field "decade")
                               year: (get-field "year")
                               month: (get-field "month")
                               day: (get-field "day"))))
       pics))))

(define (update-tag!)
  (with-request-variables (tag-id
                           original-tag
                           new-tag
                           (remove? (nonempty as-boolean)))
    (if (or (equal? new-tag "") remove?)
        (db-remove-tag! original-tag)
        (db-update-tag! original-tag new-tag))))

(define (ajax/run-ocr)
  (with-request-variables (pic-id filename language)
    (run-ocr filename language)))
