(module awful-picman-process-dir (process-dir)

(import chicken scheme)
(use extras files ports posix srfi-1)
(use awful awful-picman-params awful-picman-utils awful-picman-db)

(define (insert-missing-pics/db! dir)
  (let* ((images-in-db (db-dir-pics dir))
         (images-in-dir (map pathname-strip-directory
                             (filter image-file? (directory dir))))
         (images-to-insert (remove (lambda (img)
                                     (member img images-in-db))
                                   images-in-dir)))
    (unless (null? images-to-insert)
      (insert-multiple-pics! dir images-to-insert))))

(define (process-dir dir)
  (debug 1 "Processing ~a" dir)
  (insert-missing-pics/db! dir))

) ;; end module
