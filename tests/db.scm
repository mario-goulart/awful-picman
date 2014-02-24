(use awful sql-de-lite)
(use awful-picman-db)

(change-directory "pics")

;;; Utils
(define (sort-strings l)
  (sort l string<))

(define (pic-tags path)
  (sort-strings (db-pic-tags (get-pic-from-db path))))

;;; Some cleanup
(when (file-exists? ".awful-picman")
  (delete-directory ".awful-picman" 'recursive))

(when (file-exists? "more-pics")
  (delete-directory "more-pics" 'recursive))

;; test pics created by tests
(for-each delete-file* '("chicken2.png" "chicken3.png" "chicken4.png"))

;;; Initialize the test environment
(create-directory "more-pics")
(for-each (lambda (pic)
            (file-copy pic (make-pathname "more-pics" pic)))
          (glob "*.png"))

(system* (sprintf "~a --init-only --recursive" picman))

(db-credentials ".awful-picman/awful-picman.db")

(set! $db (lambda (q #!key default values)
            (call-with-database (db-credentials)
              (lambda (db)
                (db-query db q default: default values: values)))))

;;; Tests
(test-begin "basic DB tests")

(test-assert (file-exists? ".awful-picman/awful-picman.db"))

;; Initially (before the initialization), only the following files
;; exist on the filesystem:
;; * chicken.png
;; * chicken-upside-down.png
;; * more-pics/chicken.png
;; * more-pics/chicken-upside-down.png

(test '((1 "." "chicken.png" "" "" "" "" "")
        (2 "." "chicken-upside-down.png" "" "" "" "" "")
        (3 "./more-pics" "chicken.png" "" "" "" "" "")
        (4 "./more-pics" "chicken-upside-down.png" "" "" "" "" ""))
      ($db "select * from pics order by pic_id"))

;; Along the tests execution, new files will be created (e.g.,
;; chicken2.png, chicken3.png, chicken4.png)

(test-end "basic DB tests")

(define (test-db-pic path #!key decade year month day (descr "") (tags '()) (albums '()))
  (let ((pic (get-pic-from-db path)))
    (test-assert (db-pic? pic))
    (test decade (db-pic-decade pic))
    (test year (db-pic-year pic))
    (test month (db-pic-month pic))
    (test day (db-pic-day pic))
    (test descr (db-pic-descr pic))
    (test path (db-pic-path pic))
    (test tags (db-pic-tags pic))
    (test albums (db-pic-albums pic))))

(define (test-db-album album-obj #!key title (descr ""))
  (test-assert (db-album? album-obj))
  (test title (db-album-title album-obj))
  (test descr (db-album-descr album-obj)))

(test-begin "existing pic")
(test-db-pic "./more-pics/chicken.png")
(test-end "existing pic")

(test-begin "insertion of a new pic")
(file-copy "./chicken.png" "chicken2.png")
(insert/update-pic! "./chicken2.png")
(test-db-pic "./chicken2.png")
(test-end "insertion of a new pic")

(test-begin "updating pic")
(insert/update-pic! "./chicken2.png"
                    decade: 2010
                    year: 4
                    month: 2
                    day: 19
                    descr: "a test"
                    tags: '("foo" "bar")
                    albums: '("album1"))

(test-db-pic "./chicken2.png"
             decade: 2010
             year: 4
             month: 2
             day: 19
             descr: "a test"
             tags: '("foo" "bar")
             albums: '("album1"))

(test-begin "tags after updating pic")
(test '("bar" "foo")
      (sort-strings (db-tags)))
(test-end "tags after updating pic")

(test-begin "album after updating pic")
(test-db-album (car (db-albums)) title: "album1")
(test-end "album after updating pic")
(test-end "updating pic")

(test-begin "insertion of multiple pics (insert-multiple-pics!)")
(file-copy "chicken.png" "chicken3.png")
(file-copy "chicken.png" "chicken4.png")
(insert-multiple-pics! "." '("chicken3.png" "chicken4.png"))
(test-db-pic "./chicken3.png")
(test-db-pic "./chicken4.png")
(test-end "insertion of multiple pics (insert-multiple-pics!)")

(test-begin "updating tags (db-update-tag!)")
(db-update-tag! "foo" "foo2")
(test '("bar" "foo2")
      (sort-strings (db-tags)))
(test-end "updating tags (db-update-tag!)")

(test-begin "removing tags (db-remove-tag!)")
(test '("bar" "foo2") (pic-tags "./chicken2.png"))
(db-remove-tag! "foo2")
(test '("bar") (db-tags))
(test '("bar") (pic-tags "./chicken2.png"))
(test-end "removing tags (db-remove-tag!)")

;; So far, the current DB state is:
;; (pp ($db "select * from pics order by pic_id"))
;;
;;   ((1 "." "chicken.png" "" "" "" "" "")
;;    (2 "." "chicken-upside-down.png" "" "" "" "" "")
;;    (3 "./more-pics" "chicken.png" "" "" "" "" "")
;;    (4 "./more-pics" "chicken-upside-down.png" "" "" "" "" "")
;;    (5 "." "chicken2.png" "a test" 2010 4 2 19)
;;    (6 "." "chicken3.png" "" "" "" "" "")
;;    (7 "." "chicken4.png" "" "" "" "" ""))
;;
;; (pp ($db "select * from tags"))
;;
;;   ((5 "bar"))

;; Here we set some tags, so we can test db-tag-filter and
;; db-filter/without-tag

(test-begin "filtering tags db-tag-filter and db-filter/without-tag")

(test "Assert that ./chicken2.png is in album1"
      '("album1")
      (db-pic-albums (get-pic-from-db "./chicken2.png")))

(insert/update-pic! "./chicken2.png" tags: '("foo" "baz" "quux"))
(insert/update-pic! "./chicken3.png" tags: '("baz" "quux"))
(insert/update-pic! "./chicken4.png" tags: '("quux"))
(insert/update-pic! "./more-pics/chicken.png" tags: '("foo" "bar"))
(insert/update-pic! "./more-pics/chicken-upside-down.png" tags: '("foo" "baz"))

(test "Check that ./chicken2.png is still in album1 after insert/update-pic!"
      '("album1")
      (db-pic-albums (get-pic-from-db "./chicken2.png")))

(test '() (pic-tags "./chicken.png"))

(test '() (pic-tags "./chicken-upside-down.png"))

(test '("baz" "foo" "quux")
      (sort-strings (pic-tags "./chicken2.png")))

(test '("baz" "quux")
      (sort-strings (pic-tags "./chicken3.png")))

(test '("quux")
      (pic-tags "./chicken4.png"))

(test '("bar" "foo")
      (sort-strings (pic-tags "./more-pics/chicken.png")))

(test '("baz" "foo")
      (sort-strings (pic-tags "./more-pics/chicken-upside-down.png")))

(test '() (db-tag-filter '() '()))

(test "pics with tag foo"
      (sort-strings '("./chicken2.png"
                      "./more-pics/chicken.png"
                      "./more-pics/chicken-upside-down.png"))
      (sort-strings (db-tag-filter '("foo") '())))

(test "pics with tags foo and baz"
      (sort-strings '("./chicken2.png"
                      "./more-pics/chicken-upside-down.png"))
      (sort-strings (db-tag-filter '("foo" "baz") '())))

(test "pics with tags foo and foo2"
      '()
      (db-tag-filter '("foo" "foo2") '()))

(test "pics without tag foo"
      (sort-strings '("./chicken.png"
                      "./chicken3.png"
                      "./chicken4.png"
                      "./chicken-upside-down.png"))
      (sort-strings (db-tag-filter '() '("foo"))))

(test "pics with tag foo but without tag baz"
      '("./more-pics/chicken.png")
      (db-tag-filter '("foo") '("baz")))

(test "pics with tag foo and baz, without non existing tag"
      (sort-strings '("./chicken2.png"
                      "./more-pics/chicken-upside-down.png"))
      (sort-strings (db-tag-filter '("foo" "baz") '("blablabla"))))

(test "pics without tags"
      (sort-strings '("./chicken.png"
                      "./chicken-upside-down.png"))
      (sort-strings (db-filter/without-tag)))

(test-end "filtering tags db-tag-filter and db-filter/without-tag")

;;; Albums
(test-begin "albums")

(let ((album1 (car (db-albums))) ;; album1 is the only album so far
      (pic (get-pic-from-db "./chicken2.png"))) ;; chicken2.png is in album1
  (test "album1" (db-album-title album1))
  (test '("album1") (db-pic-albums pic))
  (db-remove-album! (db-album-id album1))
  (test '() (db-albums))
  (set! pic (get-pic-from-db "./chicken2.png"))
  (test '() (db-pic-albums pic)))

(insert/update-pic! "./chicken.png"
                    albums: '("album2" "album3"))

(test '("album2" "album3")
      (sort-strings (map db-album-title (db-albums))))

(test '("" "")
      (map db-album-descr (db-albums)))

(test '("album2" "album3")
      (sort-strings
       (db-pic-albums (get-pic-from-db "./chicken.png"))))

(test-begin "updating album (db-update-album!)")

(let ((albums (db-albums)))
  (for-each
   (lambda (album)
     (db-update-album! (db-album-id album)
                       (sprintf "the ~a" (db-album-title album))
                       (sprintf "description for ~a" (db-album-title album))))
   albums)

  (test "the album2"
        (caar ($db "select title from albums where album_id=2")))
  (test "description for album2"
        (caar ($db "select descr from albums where album_id=2")))

  (test "the album3"
        (caar ($db "select title from albums where album_id=3")))
  (test "description for album3"
        (caar ($db "select descr from albums where album_id=3"))))

(test-end "updating album (db-update-album!)")

(test-begin "counting")
(test 1 (db-album-pics-count (caar ($db "select album_id from albums where title='the album2'"))))
(insert/update-pic! "./chicken2.png" albums: '("the album1" "the album2"))
(test 1 (db-album-pics-count (caar ($db "select album_id from albums where title='the album1'"))))
(test 2 (db-album-pics-count (caar ($db "select album_id from albums where title='the album2'"))))
(test-end "counting")


(test-begin "pics without album (db-filter/without-album)")
(test (sort-strings '("./chicken-upside-down.png"
                      "./chicken3.png"
                      "./chicken4.png"
                      "./more-pics/chicken.png"
                      "./more-pics/chicken-upside-down.png"))
      (sort-strings (db-filter/without-album)))
(test-end "pics without album (db-filter/without-album)")

(test-begin "pictures in directories (db-dir-pics)")
(test (sort-strings '("chicken.png"
                      "chicken-upside-down.png"
                      "chicken2.png"
                      "chicken3.png"
                      "chicken4.png"))
      (sort-strings (db-dir-pics ".")))
(test (sort-strings '("chicken.png"
                      "chicken-upside-down.png"))
      (sort-strings (db-dir-pics "./more-pics")))
(test-end "pictures in directories (db-dir-pics)")

(test-end "albums")

(change-directory "..")
