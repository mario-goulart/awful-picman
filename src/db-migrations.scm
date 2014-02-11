;;;
;;; Migrations
;;;

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
