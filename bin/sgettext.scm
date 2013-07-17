;;; Quick & very dirty xgettext replacement for scheme files.
;;; xgettext is buggy when scheme code contains extended DSSSL style
;;; lambda lists

(use (only srfi-1 delete-duplicates))

(define *strings/files* '())

(define (add-string/file! str file)
  (let ((pair (assoc str *strings/files*)))
    (if pair
        (set-cdr! pair (cons file (cdr pair)))
        (set! *strings/files* (cons (list str file) *strings/files*)))))

(define (string->po strs file)
  (let ((str (handle-exceptions exn
               (begin
                 (with-output-to-port (current-error-port)
                   (lambda ()
                     (print-error-message exn)
                     (display file)
                     (display ": ")
                     (pp strs)
                     (exit 3))))
               (string-intersperse strs ""))))
    (add-string/file! str file)))

(define (display-po)
  (for-each
   (lambda (string/files)
     (for-each (lambda (file)
                 (print "#: " file))
               (delete-duplicates (cdr string/files)))
     (display "msgid ")
     (pp (car string/files))
     (print "msgstr \"\"\n"))
   *strings/files*))

(define (form->po form file)
  (when (and (list? form) (not (null? form)))
    (if (eq? (car form) '_)
        (string->po (cdr form) file)
        (for-each (lambda (subform)
                    (form->po subform file))
                  form))))

(define (display-po-header)
  (display #<#EOF
msgid ""
msgstr ""
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=utf-8\n"


EOF
))

(define (generate-pot files)
  (for-each
   (lambda (file)
     (let ((forms (read-file file)))
       (for-each (lambda (form)
                   (form->po form file))
                 forms)))
   files)
  (display-po))

(define (usage #!optional exit-code)
  (print (pathname-strip-directory (program-name)) " <files>")
  (when exit-code (exit exit-code)))

(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (generate-pot args))
