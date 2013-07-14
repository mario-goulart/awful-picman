(define conf-filename "awful-picman.conf")
(define dot-conf-filename (string-append "." conf-filename))

(define (load-global-conf)
  (let ((conf-file (make-pathname (get-environment-variable "HOME")
                                  dot-conf-filename)))
    (when (file-read-access? conf-file)
      (load conf-file))))

(define (load-local-conf)
  (when (file-read-access? conf-filename)
    (load conf-filename)))
