(module awful-picman-export

(export-album)

(import chicken scheme)
(use data-structures files ports posix)
(use awful json slice simple-sha1 uri-common)
(use awful-picman-params
     awful-picman-utils
     awful-picman-image
     awful-picman-db)

(define (write-index dir album-title pic-filenames)
  (with-output-to-file (make-pathname dir "index.html")
    (lambda ()
      (display
       ((sxml->html)
        `((literal "<!DOCTYPE HTML>")
          (html
           (head
            (meta (@ (http-equiv "Content-Type")
                     (content "application/xhtml+xml; charset=utf-8")))
            (title ,album-title)
            (style "ul li { display: inline; list-style-type: none }"))
           (body
            (h1 ,album-title)
            (ul
             ,@(map (lambda (pic-filename)
                      `(li (a (@ (href ,pic-filename))
                              (img (@ (src ,(make-pathname "thumbnails"
                                                           pic-filename)))))))
                    pic-filenames))))))))))

(define (export-album album-title dir hi-res? index?)
  (create-directory dir 'recursively)
  (when index?
    (create-directory (make-pathname dir "thumbnails") 'recursively))
  (let ((pic-ids/paths (db-get-pics-id/path-by-album album-title))
        (pic-filenames '()))
    (for-each
     (lambda (pic-id/path)
       (let* ((pic-id (car pic-id/path))
              (pic-path (cdr pic-id/path))
              (pic-fs-path (if hi-res?
                               pic-path
                               (thumbnail-path pic-path
                                               (thumbnails/zoom-dimension))))
              (thumbnail-fs-path
               (and index?
                    (thumbnail-path pic-path
                                    (thumbnails/small-dimension))))
              (extension (pathname-extension pic-path))
              (pic-id (number->string pic-id)))
         (set! pic-filenames (cons (make-pathname #f pic-id extension)
                                   pic-filenames))
         (when (and (not hi-res?)
                    (not (file-exists? pic-fs-path)))
           (image->thumbnail pic-path (thumbnails/zoom-dimension)))
         (when (and index?
                    (not (file-exists? thumbnail-fs-path)))
           (image->thumbnail pic-path (thumbnails/small-dimension)))
         (when index?
           (file-copy thumbnail-fs-path
                      (make-pathname (list dir "thumbnails") pic-id extension)
                      'clobber))
         (file-copy pic-fs-path
                    (make-pathname dir pic-id extension)
                    'clobber)))
     pic-ids/paths)
    (when index?
      (write-index dir album-title pic-filenames))))

) ;; end module
