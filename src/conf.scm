(define conf-filename "awful-picman.conf")
(define dot-conf-filename (string-append "." conf-filename))

(define (conf-loader scope conf-file)
  (if (file-read-access? conf-file)
      (begin
        (debug 1 "Loading ~a configuration file ~a" scope conf-file)
        (load conf-file))
      (debug 2 "NOT loading ~a configuration file ~a" scope conf-file)))

(define (load-global-conf)
  ;; How to handle that on windows?
  (conf-loader 'global
               (make-pathname "/etc" conf-filename)))

(define (load-user-conf)
  (conf-loader 'user
               (make-pathname (get-environment-variable "HOME")
                              dot-conf-filename)))

(define (load-local-conf)
  (conf-loader 'user
               (make-pathname metadata-dir conf-filename)))
