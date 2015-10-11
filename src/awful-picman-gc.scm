(module awful-picman-gc (gc!)

(import chicken scheme)
(use data-structures files posix srfi-1 utils)
(use awful matchable sql-de-lite)
(use awful-picman-params awful-picman-utils awful-picman-db)

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
              (print-error-message exn (current-error-port))
              (print-call-chain (current-error-port)))
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

(define (thumbnail->pic-path thumbnail-path)
  (let* ((parts-thumbnails-dirname (length (path-split thumbnails-dirname)))
         (parts-metadata-dir (length (path-split metadata-dir)))
         (parts-to-drop (+ 1 ;; dimension
                           parts-thumbnails-dirname
                           parts-metadata-dir)))
      (let ((path-parts (path-split thumbnail-path)))
        (path-join (cons "." (drop path-parts parts-to-drop))))))

(define (orphan-thumbnails)
  (let ((pic-files (map pathname-strip-extension
                        (find-files "."
                                    test: image-file?)))
        (thumbnails (find-files (make-pathname metadata-dir thumbnails-dirname)
                                test: image-file?)))
    (remove (lambda (thumbnail)
              (member (pathname-strip-extension (thumbnail->pic-path thumbnail))
                      pic-files))
            thumbnails)))

(define (unused-thumbnail-sets)
  (let* ((thumbnails-dir (make-pathname metadata-dir
                                        thumbnails-dirname))
         (available-thumbnail-sets
          (filter-map string->number
                      (directory thumbnails-dir)))
         (to-remove (delete (thumbnails/zoom-dimension)
                            available-thumbnail-sets
                            =)))
    (map (lambda (dim)
           (make-pathname thumbnails-dir (number->string dim)))
         to-remove)))

(define (gc!)
  ;; Orphan pics are those which exist on the database but not on the
  ;; filesystem (e.g., user deleted pics from the filesystem).  Orphan
  ;; thumbnails are thumbnails that don't have the corresponding pic file.
  (let* ((orphan-pics (db-orphan-pics))
         (orphan-thumbs (orphan-thumbnails))
         (num-orphan-pics (length orphan-pics))
         (num-orphan-thumbs (length orphan-thumbs))
         (unused-thumbnail-dirs (unused-thumbnail-sets))
         (num-unused-thumbnail-dirs (length unused-thumbnail-dirs))
         (abort&exit (lambda ()
                       (info (_ "Aborting."))
                       (exit 0))))
    (if (null? orphan-pics)
        (info (_ "No orphan pic found."))
        (begin
          (for-each info (map cdr orphan-pics))
          (info "\n~a ~a" num-orphan-pics (_ "orphan pics have been found."))
          (let ((answer
                 (yes-or-no? (_ "Remove records that reference those files from the database?")
                             abort: abort&exit)))
            (if answer
                (begin
                  (db-remove-orphan-pics! orphan-pics)
                  (info "~a ~a" num-orphan-pics (_ " records have been removed.")))
                (info (_ "Not removing records."))))))

    (if (null? orphan-thumbs)
        (info (_ "No orphan thumbnail found."))
        (begin
          (for-each info orphan-thumbs)
          (info "\n~a ~a" num-orphan-thumbs (_ "orphan thumbnails have been found."))
          (let ((answer
                 (yes-or-no? (_ "Remove orphan thumbnails?")
                             abort: abort&exit)))
            (if answer
                (begin
                  (for-each delete-file* orphan-thumbs)
                  (info "~a ~a" num-orphan-thumbs (_ " thumbnails have been removed.")))
                (info (_ "Not removing thumbnails."))))))
    (if (null? unused-thumbnail-dirs)
        (info (_ "No unused thumbnail directory found."))
        (begin
          (for-each info unused-thumbnail-dirs)
          (info "\n~a ~a"
                num-unused-thumbnail-dirs
                (_ "unused thumbnail directories found."))
          (let ((answer
                 (yes-or-no? (_ "Remove unused thumbnail directories?")
                             abort: abort&exit)))
            (if answer
                (begin
                  (for-each (lambda (dir)
                              (delete-directory dir 'recursively))
                            unused-thumbnail-dirs)
                  (info "~a ~a"
                        num-unused-thumbnail-dirs
                        (_ " unused thumbnail directories have been removed.")))
                (info (_ "Not removing unused thumbnail directories."))))))))

) ;; end module
