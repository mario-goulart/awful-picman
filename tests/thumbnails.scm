(use awful-picman-params awful-picman-image awful-picman-process-dir)

;; Assume tests for the databas has been run and created some pic
;; files

(change-directory "pics")

(test-begin "thumbnails")

;; Check if pics that existed at the time process-dir was run (i.e.,
;; during initialization) exist as thumbnails
(test-begin "pics that existed at initialization")
(for-each
 (lambda (dimension)
   (for-each
    (lambda (pic-path)
      (test-assert pic-path
                   (file-exists? (thumbnail-path pic-path dimension))))
    '("./chicken.png"
      "./chicken-upside-down.png"
      "./more-pics/chicken.png"
      "./more-pics/chicken-upside-down.png")))
 (thumbnails/max-dimensions))
(test-end "pics that existed at initialization")


;; Check if pics that didn't exist during initialization do _not_
;; exist as thumbnails.
(test-begin "pics that didn't exist at initialization")
(for-each
 (lambda (dimension)
   (for-each
    (lambda (pic-path)
      (test-assert pic-path
                   (not (file-exists? (thumbnail-path pic-path dimension)))))
    '("./chicken2.png"
      "./chicken3.png"
      "./chicken4.png")))
 (thumbnails/max-dimensions))
(test-end "pics that didn't exist at initialization")


;; Check if thumbnails in more-pics are not created when calling the
;; non-recursive process-dir

(test-begin "thumbnails status after process-dir (non-recursive)")

(process-dir "." (find-missing-thumbnails "."))

(for-each
 (lambda (dimension)
   (for-each
    (lambda (dir)
      (let ((pics (db-dir-pics dir)))
        (for-each
         (lambda (pic)
           (let ((pic-path (make-pathname dir pic)))
             (if (or (equal? dir ".")
                     (member pic-path '("./more-pics/chicken.png"
                                        "./more-pics/chicken-upside-down.png")))
                 ;; Files that existed at initialization
                 (test-assert pic-path
                              (file-exists? (thumbnail-path pic-path dimension)))
                 (test-assert pic-path
                              (not (file-exists? (thumbnail-path pic-path dimension)))))))
         pics)))
    '("." "./more-pics")))
 (thumbnails/max-dimensions))
(test-end "thumbnails status after process-dir (non-recursive)")


;; Check all thumbnails are created when calling the recursive
;; process-dir

(process-dir/recursive "." (find-missing-thumbnails "."))

(test-begin "thumbnails status after process-dir (recursive)")
(for-each
 (lambda (dimension)
   (for-each
    (lambda (dir)
      (let ((pics (db-dir-pics dir)))
        (for-each
         (lambda (pic)
           (let ((pic-path (make-pathname dir pic)))
             (test-assert pic-path
                          (file-exists? (thumbnail-path pic-path dimension)))))
         pics)))
    '("." "./more-pics")))
 (thumbnails/max-dimensions))
(test-end "thumbnails status after process-dir (recursive)")

(test-end "thumbnails")

(change-directory "..")
