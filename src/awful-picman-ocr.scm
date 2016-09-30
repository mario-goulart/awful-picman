(module awful-picman-ocr

  (ocr-supported-pic-format?
   ocr-installed?
   run-ocr
   )

(import chicken scheme)
(use files ports posix utils srfi-13)
(use awful-picman-params awful-picman-utils)

(define (ocr-supported-pic-format? pic-file)
  (let ((extension (pathname-extension pic-file)))
    (and (member (string-downcase extension)
                 (map string-downcase (ocr-supported-formats)))
         #t)))

(define (ocr-installed?)
  (program-available? (ocr-program)))

(define (run-ocr pic-file lang)
  (let-values (((temp-fd temp-file) (file-mkstemp "ocr-out.XXXXXX")))
    (handle-exceptions exn
      (begin
        (delete-file* temp-file)
        (info-error "Error running OCR on ~a: ~a"
                    pic-file
                    (with-output-to-string
                      (lambda ()
                        (print-error-message exn))))
        "")
      (let ((cmd
             (sprintf "~a ~a > ~a 2>&1"
                      (ocr-program)
                      ((ocr-args) pic-file temp-file lang)
                      (if (eq? (software-type) 'windows)
                          "NUL"
                          "/dev/null"))))
        (debug 2 "Running OCR: ~a" cmd)
        (system* cmd)
        (let* ((text-file (make-pathname #f temp-file ".txt"))
               (text (with-input-from-file text-file read-all)))
          (delete-file temp-file)
          (delete-file text-file)
          text)))))

) ;; end module
