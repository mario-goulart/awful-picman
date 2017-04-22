#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

;; This script inserts dates for pics based on their exif data.  It
;; gets the path to pictures from the database and try to get exif
;; dates from those files (it doesn't scan picture files that are not
;; in the database).

(use data-structures extras files posix)
(use exif sql-de-lite)
(use awful-picman-db awful-picman-params awful-picman-utils)

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port "Usage: ~a [<directory>]\n" this)
    (when exit-code
      (exit exit-code))))

(define (insert-exif-date! db-file)
  (define limit 10)
  (call-with-database db-file
    (lambda (db)
      (let loop ((offset 0))
        (let* ((raw-pics
                (db-query db
                          (string-append
                           "select pic_id, dir, filename, descr, decade, year, month, day "
                           "from pics order by pic_id limit ? offset ?")
                          values: (list limit offset)))
               (pics (map (lambda (raw-pic)
                            (apply make-db-pic (append raw-pic (list '() '()))))
                          raw-pics))
               (null-date? (lambda (d) (or (null? d) (equal? d "")))))
          (for-each
           (lambda (pic)
             (when (and (null-date? (db-pic-decade pic))
                        (null-date? (db-pic-year pic))
                        (null-date? (db-pic-month pic))
                        (null-date? (db-pic-day pic)))
               (let ((exif-date (get-exif-date-from-file (db-pic-path pic))))
                 (when exif-date
                   (info "~a (~a) -> ~a-~a-~a"
                         (db-pic-path pic)
                         (db-pic-id pic)
                         (+ (car exif-date) (cadr exif-date))
                         (caddr exif-date)
                         (cadddr exif-date))
                   (db-query db "update pics set decade=?, year=?, month=?, day=? where pic_id=?"
                             values: (append exif-date (list (db-pic-id pic))))))))
           pics)
          (unless (null? pics)
            (loop (- (+ offset limit) 1))))))))

(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (unless (null? args)
    (change-directory (car args)))

  (let ((db-file (make-pathname metadata-dir db-filename)))
    (unless (file-exists? db-file)
      (info-error "Could not find database file (~a).  Aborting." db-file)
      (exit 1))
    (insert-exif-date! db-file)))
