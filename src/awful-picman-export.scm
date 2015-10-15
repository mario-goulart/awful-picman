(module awful-picman-export

(export-album)

(import chicken scheme)
(use data-structures files ports posix)
(use awful json slice simple-sha1 uri-common)
(use awful-picman-params
     awful-picman-utils
     awful-picman-image
     awful-picman-db)

(define (export-album album-title dir hi-res?)
  (create-directory dir 'recursively)
  (for-each
   (lambda (pic-id/path)
     (let* ((pic-id (car pic-id/path))
            (pic-path (cdr pic-id/path))
            (pic-fs-path (if hi-res?
                             (cdr pic-id/path)
                             (thumbnail-path pic-path
                                             (thumbnails/zoom-dimension)))))
       (when (and (not hi-res?)
                  (not (file-exists? pic-fs-path)))
         (image->thumbnail pic-path (thumbnails/zoom-dimension)))
       (file-copy pic-fs-path
                  (make-pathname dir
                                 (number->string pic-id)
                                 (pathname-extension pic-path))
                  'clobber)))
   (db-get-pics-id/path-by-album album-title)))

) ;; end module
