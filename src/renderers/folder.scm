(define (render-dir-stat dir)
  (define (describe count obj)
    (case count
      ((0) (string-append (_ "no ") obj))
      ((1) (string-append "1 " obj))
      (else (sprintf "~a ~as" count obj))))
  (let ((stat (get-dir-stat dir)))
    `(small ,(string-intersperse
              (list (describe (dir-stat-num-pics stat) (_ "pic"))
                    (describe (dir-stat-num-dirs stat) (_ "folder"))
                    (describe (dir-stat-num-files stat) (_ "file")))
              ", "))))

(define (render-dir-link dir-obj)
  (let* ((dir (thumb-dir dir-obj))
         (dirname (thumb-filename dir-obj))
         (web-path (make-pathname (list (folders-web-dir) dir)
                                  dirname))
         (size (default-thumbnail-dimension)))
    `(div (@ (class "dir"))
          (a (@ (href ,web-path))
             (img (@ (src "/img/dir.png") (alt ,dirname))))
          (p (a (@ (href ,web-path)) ,dirname))
          ,(render-dir-stat (make-pathname (list root-dir dir) dirname)))))

(define (render-other-file-type filename)
  (let ((size (default-thumbnail-dimension)))
    `(div (@ (class "other-file-type"))
          (img (@ (src "/img/unknown.png") (alt ,filename)))
          (p ,filename))))

(define-record thumb type dir filename idx)

(define-record-printer (thumb obj out)
  (fprintf out "#<thumb type=~a dir=~a filename=~a idx=~a>"
           (thumb-type obj)
           (thumb-dir obj)
           (thumb-filename obj)
           (thumb-idx obj)))

(define (next-thumb current pics num-pics)
  (let ((idx (thumb-idx current)))
    (if (= idx (- num-pics 1))
        #f
        (thumb-filename (list-ref pics (+ idx 1))))))

(define (prev-thumb current pics num-pics)
  (let ((idx (thumb-idx current)))
    (if (zero? idx)
        #f
        (thumb-filename (list-ref pics (- idx 1))))))


(define (paginate-folder dir-items page-num)

  (define (make-dir path)
    (make-thumb 'dir (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define (make-other path)
    (make-thumb 'other (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define (make-pic path idx)
    (make-thumb 'pic (pathname-directory path) (pathname-strip-directory path) idx))

  (let* ((offset (* page-num (thumbnails/page)))
         (page-items (slice dir-items offset (+ offset (thumbnails/page))))
         (dirs (map make-dir (filter directory? page-items)))
         (pics (let ((pics (filter image-file? page-items)))
                 (map make-pic pics (iota (length pics)))))
         (other (map make-other
                     (remove (lambda (i)
                               (or (directory? i)
                                   (image-file? i)))
                             page-items)))
         (thumbs (append dirs pics other)))
    thumbs))


(define (render-dir-content dir page-num)
  (let* ((dir-items (list-directory dir))
         (num-dir-items (length dir-items))
         (thumbs (paginate-folder dir-items page-num)))
    `(,(render-breadcrumbs dir (_ "Folders") (folders-web-dir))
      ,(render-thumbnails thumbs 'folder)
      ,(render-pagination-links num-dir-items page-num))))
