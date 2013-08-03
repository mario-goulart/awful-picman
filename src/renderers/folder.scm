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

(define (render-dir-content dir)

  (define (make-dir path)
    (make-thumb 'dir (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define (make-other path)
    (make-thumb 'other (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define make-pic
    (let ((idx -1))
      (lambda (path)
        (set! idx (+ idx 1))
        (make-thumb 'pic (pathname-directory path) (pathname-strip-directory path) idx))))

  (let* ((items (glob (make-pathname dir "*")))
         (dirs (map make-dir (filter directory? items)))
         (pics (map make-pic (filter image-file? items)))
         (other (map make-other
                     (remove (lambda (i)
                               (or (directory? i)
                                   (image-file? i)))
                             items)))
         (items (append dirs pics other)))
    `(,(render-breadcrumbs dir (_ "Folders") (folders-web-dir))
      ,(render-thumbnails items 'folder))))
