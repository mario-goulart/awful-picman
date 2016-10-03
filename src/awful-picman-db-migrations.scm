(module awful-picman-db-migrations (maybe-migrate-db!)

(import chicken scheme)
(use srfi-1)
(use awful sql-de-lite)
(use awful-picman-utils awful-picman-db)

(define *db-migrations* '())

(define (add-migration! next-version migration-proc)
  (let ((migration
         (lambda ()
           (call-with-database (db-credentials)
             (lambda (db)
               (with-transaction db
                 (lambda ()
                   (migration-proc db)
                   (db-query db "delete from version")
                   (db-query db "insert into version (version) values (?)"
                             values: (list next-version))
                   #t)))))))
    (set! *db-migrations* (cons migration *db-migrations*))))


;;; IMPORTANT: migrations must be added in the order they are to be
;;; applied!

;; version 0 -> 1
(add-migration!
 1
 (lambda (db)
   ;; version 0 didn't have a version table
   (db-query db "create table version (version integer)")))

;; version 1 -> 2
(add-migration!
 2
 (lambda (db)
   ;; Make records in albums_pics and tags tables unique
   (db-query db "create table tmp1 (pic_id integer, album_id integer)")
   (db-query db "insert into tmp1 select distinct * from albums_pics")
   (db-query db "drop table albums_pics")
   (db-query db "create table albums_pics (
                   pic_id integer,
                   album_id integer,
                   constraint uniq primary key (pic_id, album_id))")
   (db-query db "insert into albums_pics select * from tmp1")
   (db-query db "drop table tmp1")

   (db-query db "create table tmp2 (pic_id integer, tag text)")
   (db-query db "insert into tmp2 select distinct * from tags")
   (db-query db "drop table tags")
   (db-query db "create table tags (
                   pic_id integer,
                   tag text,
                   constraint uniq primary key (pic_id, tag))")
   (db-query db "insert into tags select * from tmp2")
   (db-query db "drop table tmp2")
   ))


;;; end migrations

(define (version-table-exists?)
  ;; The first db version didn't have a version table
  (call-with-database (db-credentials)
    (lambda (db)
      (not
       (null?
        (db-query db "select name from sqlite_master where type='table' and name='version'"))))))

(define (current-db-version)
  (if (version-table-exists?)
      (call-with-database (db-credentials)
        (lambda (db)
          (caar (db-query db "select version from version"))))
      0))

(define (maybe-migrate-db!)
  (let ((version (current-db-version))
        (total-migrations (length *db-migrations*)))
    (debug 1 "Current DB schema version is ~S" version)
    (assert (<= version total-migrations))
    (let ((migrations (drop (reverse *db-migrations*) version)))
      (for-each
       (lambda (migration version)
         (let ((next-version (+ version 1)))
           (info "Running migration from version ~a to ~a..." version next-version)
           (handle-exceptions exn
             (begin
               (print-call-chain (current-error-port))
               (print-error-message exn (current-error-port))
               (info-error "Error running migration from version ~a to ~a" version next-version)
               (exit 1))
             (migration))
           (info "Successfully migrated from version ~a to ~a" version next-version)))
       migrations
       (iota (- total-migrations version) version)))))

) ;; end module
