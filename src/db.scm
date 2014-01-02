(define (initialize-database db-file force?)
  (when (or (not (file-exists? db-file))
            force?)
    (info "Initializing database ~a" db-file)
    (with-output-to-file db-file (cut display ""))
    (let ((db (open-database db-file)))

      ;; Pics table
      (exec (sql db "
create table pics (
    pic_id integer primary key autoincrement,
    dir text,
    filename,
    descr text,
    decade integer,
    year integer,
    month integer,
    day integer)"))

      ;; Tags table
      (exec (sql db "
create table tags (
    pic_id,
    tag text)"))

      ;; Albums table
      (exec (sql db "
create table albums (
    album_id integer primary key autoincrement,
    title text,
    descr text)"))

      ;; Albums & pics
      (exec (sql db "
create table albums_pics (
    pic_id integer,
    album_id integer)"))
      (close-database db))))

(define (db-query db-conn q #!key (values '()))
  (apply query (append (list (map-rows (lambda (data) data))
                             (sql db-conn q))
                       values)))

(define (update-pic-data! db pic-id descr decade year month day tags albums)
  ;; This is ugly:
  (when descr
    (db-query db "update pics set descr=? where pic_id=?"
              values: (list descr pic-id)))
  (when decade
    (db-query db "update pics set decade=? where pic_id=?"
              values: (list decade pic-id)))
  (when year
    (db-query db "update pics set year=? where pic_id=?"
              values: (list year pic-id)))
  (when month
    (db-query db "update pics set month=? where pic_id=?"
              values: (list month pic-id)))
  (when day
    (db-query db "update pics set day=? where pic_id=?"
              values: (list day pic-id)))
  ;; update tags
  (debug 2 "update-pics-data!: tags: ~S" tags)
  (db-query db "delete from tags where pic_id=?"
            values: (list pic-id))
  (insert-tags! db pic-id tags)

  ;; update albums
  (debug 2 "update-pics-data!: albums: ~S" albums)
  (db-query db "delete from albums_pics where pic_id=?"
            values: (list pic-id))
  (insert-albums! db pic-id albums))

(define (insert-pic-data! db dir filename descr decade year month day tags albums)
  (db-query db "insert into pics (dir, filename, descr, decade, year, month, day) values (?, ?, ?, ?, ?, ?, ?)"
            values: (list dir
                          filename
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
  (let ((dir (or (pathname-directory path) "."))
        (filename (pathname-strip-directory path)))
    (call-with-database (db-credentials)
      (lambda (db)
        (with-transaction db
          (lambda ()
            (or (and-let* ((data (db-query db "select pic_id from pics where dir=? and filename=?"
                                           values: (list dir filename)))
                           ((not (null? data)))
                           (pic-id (caar data)))
                  ;; pic is in db.  Update its data.
                  (update-pic-data! db pic-id descr decade year month day tags albums))

                ;; pic is NOT in db.  Add it.
                (insert-pic-data! db dir filename descr decade year month day tags albums))
            #t))))))

(define (insert-multiple-pics! dir pics)
  (let* ((num-query-args 7)
         (max-query-args 999) ;; default value for SQLITE_MAX_VARIABLE_NUMBER
         (max-args/query (inexact->exact (floor (/ max-query-args num-query-args))))
         (pics-slices (chop pics max-args/query)))
    (for-each
     (lambda (pics)
       (let ((query
              (string-append
               "insert into pics (dir, filename, descr, decade, year, month, day) values "
               (string-intersperse (map (lambda (dummy) "(?, ?, ?, ?, ?, ?, ?)") pics) ",")))
             (values (let loop ((pics pics))
                       (if (null? pics)
                           '()
                           (append (list dir (car pics) "" "" "" "" "")
                                   (loop (cdr pics)))))))
         (call-with-database (db-credentials)
                             (lambda (db)
                               (db-query db query values: values)))))
     pics-slices)))

(define-record db-pic id dir filename descr decade year month day tags albums)

(define-record-printer (db-pic obj out)
  (fprintf out "#<db-pic id=~a dir=~a filename=~a descr=~a decade=~a year=~a month=~a day=~a tags=~S albums=~S>"
           (db-pic-id obj)
           (db-pic-dir obj)
           (db-pic-filename obj)
           (db-pic-descr obj)
           (db-pic-decade obj)
           (db-pic-year obj)
           (db-pic-month obj)
           (db-pic-day obj)
           (db-pic-tags obj)
           (db-pic-albums obj)))

(define (db-pic-path db-pic)
  (make-pathname (db-pic-dir db-pic) (db-pic-filename db-pic)))

(define (get-pic-from-db path)
  (let ((dir (pathname-directory path))
        (filename (pathname-strip-directory path)))
    (or (and-let* ((data* ($db "select pic_id, descr, decade, year, month, day from pics where dir=? and filename=?"
                               values: (list dir filename)))
                   ((not (null? data*)))
                   (data (car data*))
                   (id (car data))
                   ($ (lambda (pos) (list-ref data pos)))
                   (tags ($db "select tag from tags where pic_id=?"
                              values: (list id)))
                   (albums ($db (string-append
                                 "select albums.title from albums, albums_pics where "
                                 "albums_pics.pic_id=? and albums.album_id=albums_pics.album_id")
                                values: (list id))))
          (make-db-pic id
                       dir
                       filename
                       ($ 1)
                       (maybe-string-null->false ($ 2))
                       (maybe-string-null->false ($ 3))
                       (maybe-string-null->false ($ 4))
                       (maybe-string-null->false ($ 5))
                       (if (null? tags) '() (map car tags))
                       (if (null? albums) '() (map car albums))))
        (make-db-pic #f dir filename "" #f #f #f #f '() '()))))

;;;
;;; Tags
;;;
(define (db-tags)
  (map car ($db "select distinct tag from tags order by tag")))

(define (insert-tags! db pic-id tags)
  (for-each (lambda (tag)
              (db-query db "insert into tags (pic_id, tag) values (?, ?)"
                        values: (list pic-id tag)))
            (if tags
                (map string-trim-both tags)
                '())))

(define (db-remove-tag! tag)
  (debug 2 "db-remove-tag!: tag: ~S" tag)
  ($db "delete from tags where tag=?"
       values: (list tag)))

(define (db-update-tag! original-tag new-tag)
  (debug 2 "db-update-tag!: original-tag: ~S  new-tag: ~S" original-tag new-tag)
  ($db "update tags set tag=? where tag=?"
       values: (list new-tag original-tag)))

(define (db-tag-filter include-tags exclude-tags)
  (define (select-pics tags op)
    (string-intersperse
     (map (lambda (_)
            "select pic_id from tags where tag=?")
          (iota (length tags)))
     (sprintf " ~a " op)))
  (let ((query (string-append
                "select pics.dir, pics.filename from pics where pic_id in ("
                (select-pics include-tags "intersect")
                (if (null? exclude-tags)
                    ""
                    (string-append
                     " except "
                     (select-pics exclude-tags "except")))
                ") order by pic_id")))
    (debug 2 "db-tag-filter: query: ~S" query)
    (map (lambda (dir/f)
           (make-pathname (car dir/f) (cadr dir/f)))
         ($db query values: (append include-tags exclude-tags)))))

(define (db-filter/without-album)
  (map (lambda (dir/f)
         (make-pathname (car dir/f) (cadr dir/f)))
       ($db "select dir, filename from pics where pics.pic_id not in (select distinct pic_id from albums_pics)")))

;;;
;;; Albums
;;;

(define-record db-album id title descr)

(define-record-printer (db-album obj out)
  (fprintf out "#<db-album id=~a title=~a descr=~a>"
           (db-album-id obj)
           (db-album-title obj)
           (db-album-descr obj)))

(define (insert-albums! db pic-id albums)
  (for-each
   (lambda (album)
     (let ((db-album (db-query db "select album_id from albums where title=?"
                               values: (list album))))
       (when (null? db-album)
         (db-query db "insert into albums (title) values (?)"
                   values: (list album)))
       (let ((album-id
              (caar (db-query db "select album_id from albums where title=?"
                              values: (list album)))))
         (db-query db "insert into albums_pics (pic_id, album_id) values (?, ?)"
                   values: (list pic-id album-id)))))
   (or albums '())))

(define (db-albums)
  (map (lambda (album/descr)
         (apply make-db-album album/descr))
       ($db "select distinct album_id, title, descr from albums order by title")))

(define (db-album-pics-count album-id)
  (let ((count
         ($db (string-append
               "select count(albums_pics.pic_id) from albums, albums_pics where "
               "albums_pics.album_id=albums.album_id and albums.album_id=?")
              values: (list album-id))))
    (if (null? count)
        0
        (caar count))))

(define (db-album-pics album)
  (map (lambda (dir/f)
         (make-pathname (car dir/f) (cadr dir/f)))
       ($db (string-append
             "select pics.dir, pics.filename from pics, albums, albums_pics "
             "where albums.title=? and "
             "albums.album_id=albums_pics.album_id and "
             "pics.pic_id=albums_pics.pic_id "
             "order by pics.pic_id")
            values: (list album))))

(define (db-dir-pics dir)
  (call-with-database (db-credentials)
     (lambda (db)
       (map car (db-query db "select filename from pics where dir=?"
                          values: (list dir))))))

(define (db-remove-album! album-id)
  ($db "delete from albums where album_id=?"
       values: (list album-id)))

(define (db-update-album! album-id descr)
  ($db "update albums set descr=? where album_id=?"
       values: (list descr album-id)))
