(use files posix setup-api)
(use test)

(define picman
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f ;; salmonella adds its REPO_PREFIX/bin to PATH
       (program-path))
   "awful-picman"))

(test-begin "awful-picman")
(include "db.scm")
(include "thumbnails.scm")
(test-end "awful-picman")

(test-exit)
