(define (initialize-database db-file force?)
  (when (or (not (file-exists? db-file))
            force?)
    (info "Initializing database ~a" db-file)
    (with-output-to-file db-file (cut display ""))
    (let ((db (open-database db-file)))
      (exec (sql db "
create table files (
    pic_id integer primary key autoincrement,
    path text,
    descr text,
    decade integer,
    year integer,
    month integer,
    day integer)"))
      (exec (sql db "
create table tags (
    pic_id,
    tag text)"))
      (exec (sql db "
create table albums (
    pic_id,
    album text)"))
      (close-database db))))

(define (db-query db-conn q #!key (values '()))
  (apply query (append (list (map-rows (lambda (data) data))
                             (sql db-conn q))
                       values)))

(define (insert-tags! db pic-id tags)
  (for-each (lambda (tag)
              (db-query db "insert into tags (pic_id, tag) values (?, ?)"
                        values: (list pic-id tag)))
            (or tags '())))

(define (insert-albums! db pic-id albums)
  (for-each (lambda (album)
              (db-query db "insert into albums (pic_id, album) values (?, ?)"
                        values: (list pic-id album)))
            (or albums '())))

(define (update-pic-data! db pic-id descr decade year month day tags albums)
  ;; This is ugly:
  (when descr
    (db-query db "update files set descr=? where pic_id=?"
              values: (list descr pic-id)))
  (when decade
    (db-query db "update files set decade=? where pic_id=?"
              values: (list decade pic-id)))
  (when year
    (db-query db "update files set year=? where pic_id=?"
              values: (list year pic-id)))
  (when month
    (db-query db "update files set month=? where pic_id=?"
              values: (list month pic-id)))
  (when day
    (db-query db "update files set day=? where pic_id=?"
              values: (list day pic-id)))
  ;; update tags
  (debug "======================================= tags: ~S" tags)
  (db-query db "delete from tags where pic_id=?"
            values: (list pic-id))
  (insert-tags! db pic-id tags)

  ;; update albums
  (debug "======================================= albums: ~S" albums)
  (db-query db "delete from albums where pic_id=?"
            values: (list pic-id))
  (insert-albums! db pic-id albums))

(define (insert-pic-data! db path descr decade year month day tags albums)
  (db-query db "insert into files (path, descr, decade, year, month, day) values (?, ?, ?, ?, ?, ?)"
            values: (list path
                          (or descr "")
                          (or decade "")
                          (or year "")
                          (or month "")
                          (or day "")))
  (let ((pic-id (last-insert-rowid db)))
    (insert-tags! db pic-id tags)
    (insert-albums! db pic-id albums)))

(define (insert/update-pic! path #!key descr
                                       decade
                                       year
                                       month
                                       day
                                       tags
                                       albums)
  ;; This procedure doesn't use awful-sql-de-lite stuff because it can
  ;; be called before awful is started
  (call-with-database (db-credentials)
    (lambda (db)
      (with-transaction db
        (lambda ()
          (or (and-let* ((data (db-query db "select pic_id from files where path=?"
                                         values: (list path)))
                         ((not (null? data)))
                         (pic-id (caar data)))
                ;; pic is in db.  Update its data.
                (update-pic-data! db pic-id descr decade year month day tags albums))

              ;; pic is NOT in db.  Add it.
              (insert-pic-data! db path descr decade year month day tags albums))
          #t)))))

(define-record db-pic id path descr decade year month day tags albums)

(define (get-pic-from-db path)
  (or (and-let* ((data* ($db "select pic_id, descr, decade, year, month, day from files where path=?"
                             values: (list path)))
                 ((not (null? data*)))
                 (data (car data*))
                 (id (car data))
                 ($ (lambda (pos) (list-ref data pos)))
                 (tags ($db "select tag from tags where pic_id=?"
                            values: (list id)))
                 (albums ($db "select album from albums where pic_id=?"
                              values: (list id))))
        (make-db-pic id
                     path
                     ($ 1)
                     (maybe-string-null->false ($ 2))
                     (maybe-string-null->false ($ 3))
                     (maybe-string-null->false ($ 4))
                     (maybe-string-null->false ($ 5))
                     (if (null? tags) '() (map car tags))
                     (if (null? albums) '() (map car albums))))
      (make-db-pic #f path "" #f #f #f #f '() '())))

(define (db-tags)
  (map car ($db "select distinct tag from tags order by tag")))

(define (db-albums)
  (map car ($db "select distinct album from albums order by album")))

(define (db-album-pics-count album)
  (let ((count ($db "select count(pic_id) from albums where album=?"
                    values: (list album))))
    (if (null? count)
        0
        (caar count))))
