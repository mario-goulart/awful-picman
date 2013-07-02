(define (initialize-database db-file force?)
  (when (or (not (file-exists? db-file))
            force?)
    (info "Initializing database ~a" db-file)
    (with-output-to-file db-file (cut display ""))
    (let ((db (open-database db-file)))
      (exec (sql db "
create table files (
    file_id integer primary key autoincrement,
    path text,
    descr text,
    year integer,
    month integer,
    day integer)"))
      (exec (sql db "
create table tags (
    file_id,
    tag text)"))
      (close-database db))))

(define (db-query db-conn q #!key (values '()))
  (apply query (append (list
                        (map-rows (lambda (data) data))
                        (sql db-conn q))
                       values)))

(define (db/maybe-insert-file! path #!key (descr "")
                                          (year "")
                                          (month "")
                                          (day "")
                                          (tags '()))
  ;; This procedure doesn't use awful-sql-de-lite stuff because it can
  ;; be called before awful is started
  (call-with-database (db-credentials)
    (lambda (db)
      (with-transaction db
        (lambda ()
          (let ((inserted?
                 (not (null? (db-query db "select path from files where path=?"
                                       values: (list path))))))
            (unless inserted?
              (db-query db "insert into files (path, descr, year, month, day) values (?, ?, ?, ?, ?)"
                        values: (list path descr year month day))
              (let ((file-id (last-insert-rowid db)))
                (for-each (lambda (tag)
                            (db-query db "insert into tags (file_id, tag) values (?, ?)"
                                      values: (list file-id tag)))
                          tags))))
          #t)))))
