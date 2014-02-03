(define (db-orphan-pics)
  (let ((db-pics
         (call-with-database (db-credentials)
           (lambda (db)
             (db-query db "select pic_id, dir, filename from pics")))))
    (filter-map (match-lambda
                 ((pic-id dir filename)
                  (let ((pic-path (make-pathname dir filename)))
                    (if (file-exists? pic-path)
                        #f
                        (cons pic-id pic-path)))))
                db-pics)))

(define (db-remove-orphan-pics! orphan-pics)
  (define (format-placeholders orphan-pics)
    (string-append " ("
                   (string-intersperse
                    (map (lambda _ "?") (iota (length orphan-pics)))
                    ",")
                   ")"))
  (call-with-database (db-credentials)
    (lambda (db)
      (with-transaction db
        (lambda ()
          (handle-exceptions exn
            (begin
              (info-error (_ "An error has occurred while performing db garbage collection:"))
              (print-error-message exn (current-error-port)))
            ;; Work around the apply limit...
            (let ((slices (if (> (length orphan-pics) 120)
                              (chop orphan-pics 120)
                              (list orphan-pics))))
              (for-each
               (lambda (slice)
                 (let ((placeholders (format-placeholders slice))
                       (pic-ids (map car slice)))
                   (db-query db (string-append "delete from albums_pics where pic_id in"
                                               placeholders)
                             values: pic-ids)
                   (db-query db (string-append "delete from tags where pic_id in"
                                               placeholders)
                             values: pic-ids)
                   (db-query db (string-append "delete from pics where pic_id in"
                                               placeholders)
                             values: pic-ids)))
               slices)
              #t)))))))

(define (orphan-thumbnails orphan-pic-paths)
  (filter file-exists?
          (flatten
           (map (lambda (dimension)
                  (map (lambda (pic-path)
                         (thumbnail-path pic-path dimension))
                       orphan-pic-paths))
                (thumbnails/max-dimensions)))))

(define (gc!)
  ;; Orphan pics are those which exist on the database but not on the
  ;; filesystem (e.g., user deleted pics from the filesystem).  Orphan
  ;; thumbnails the thumbnails corresponding to orphan pics.

  ;; FIXME: At the moment, the garbage collector doesn't check whether
  ;; each thumbnail on the filesystem has a corresponding pic on the
  ;; database or on the filesystem (the actual pic file)
  (let* ((orphan-pics (db-orphan-pics))
         (orphan-thumbs (orphan-thumbnails (map cdr orphan-pics)))
         (num-orphan-pics (length orphan-pics))
         (num-orphan-thumbs (length orphan-thumbs)))
    (if (null? orphan-pics)
        (info (_ "No orphan pics found."))
        (begin
          (for-each info (map cdr orphan-pics))
          (info "\n~a ~a" num-orphan-pics (_ "orphan pics have been found."))
          (let ((answer
                 (yes-or-no? (_ "Remove records that reference those files from the database?")
                             abort: (lambda ()
                                      (info (_ "Aborting."))
                                      (exit 0)))))
            (if answer
                (begin
                  (db-remove-orphan-pics! orphan-pics)
                  (info "~a ~a" num-orphan-pics (_ " records have been removed.")))
                (info (_ "Not removing records."))))

          (for-each info orphan-thumbs)
          (info "\n~a ~a" num-orphan-thumbs (_ "orphan thumbnails have been found."))
          (let ((answer
                 (yes-or-no? (_ "Remove orphan thumbnails?")
                             abort: (lambda ()
                                      (info (_ "Aborting."))
                                      (exit 0)))))
            (if answer
                (begin
                  (for-each delete-file* orphan-thumbs)
                  (info "~a ~a" num-orphan-thumbs (_ " thumbnails have been removed.")))
                (info (_ "Not removing thumbnails."))))))))
