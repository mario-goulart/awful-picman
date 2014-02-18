(module awful-picman-process-dir

  (find-missing-thumbnails
   process-dir
   process-dir/recursive
   poll-thumbnails-conversion)

(import chicken scheme)
(use extras files ports posix srfi-1)
(use awful awful-picman-params awful-picman-utils awful-picman-db awful-picman-image)

(define (find-missing-thumbnails dir)
  (define (find-missing-thumbnails* dir dimension)
    (let ((image-files (filter image-file? (glob (make-pathname dir "*")))))
      (filter-map
       (lambda (image-file)
         (let ((thumbnail (thumbnail-path image-file dimension)))
           (and (not (file-read-access? thumbnail))
                (list image-file thumbnail dimension))))
       image-files)))
  (apply append
         (map (lambda (dim)
                (find-missing-thumbnails* dir dim))
              (thumbnails/max-dimensions))))

(define (insert-missing-pics/db! dir missing-thumbnails)
  (let* ((images-in-db (db-dir-pics dir))
         (images (map pathname-strip-directory
                      (delete-duplicates
                       (map car missing-thumbnails)
                       equal?)))
         (images-to-insert (remove (lambda (img)
                                     (member img images-in-db))
                                   images)))
    (unless (null? images-to-insert)
      (insert-multiple-pics! dir images-to-insert))))

(define (process-dir dir missing-thumbnails #!optional fork?)
  (debug 1 "Processing ~a" dir)
  (insert-missing-pics/db! dir missing-thumbnails)
  (images->thumbnails missing-thumbnails fork?))

(define (process-dir/recursive dir missing-thumbnails)
  ;; for initialization when --recursive is given on the command line
  (let ((dirs (filter directory? (glob (make-pathname dir "*")))))
    (debug 1 "  directories: ~S" dirs)
    (process-dir dir missing-thumbnails)
    (for-each (lambda (subdir)
                (process-dir/recursive subdir
                                       (find-missing-thumbnails subdir)))
              dirs)))

(define (poll-thumbnails-conversion dir target-page missing-thumbnails)
  ;; Generate the page to be displayed while images files are
  ;; converted to thumbnails
  (let ((progress-file (process-dir dir missing-thumbnails #t))
        (target-page
         (string-append "?" (append-to-query-string '((done . "true"))))))
    (periodical-ajax "/generate-thumbnails" 700
      (lambda ()
        (with-request-variables (progress-file)
          (handle-exceptions exn
            (begin
              (debug 2 "Error reading progress data: ~a"
                     (with-output-to-string
                       (lambda ()
                         (print-error-message exn))))
              '((conv-progress . "")
                (status . running)))
            (if (file-read-access? progress-file)
                (let* ((progress-data (with-input-from-file progress-file read))
                       (image-file (car progress-data))
                       (dimension (cadr progress-data))
                       (total (caddr progress-data))
                       (current (cadddr progress-data))
                       (num-dimensions (length (thumbnails/max-dimensions))))
                  `((conv-progress . ,(flonum->fixnum (/ (* current 100) total)))
                    (image-file . ,image-file)
                    (dimension . ,dimension)
                    (total . ,(/ total num-dimensions))
                    (current . ,(flonum->fixnum (/ current num-dimensions)))
                    (status . running)))
                `((conv-progress . "")
                  (status . done))))))
      update-targets: #t
      arguments: `((progress-file . ,(sprintf "'~a'" progress-file)))
      success: (sprintf
                (string-append
                 "var progress_data;"
                 "if (response['status'] == 'done') {"
                 "    window.location.replace('~a')"
                 "} else if ((response['status'] == 'running') && response['conv-progress']){"
                 "    $('.bar').width(response['conv-progress'] + '%');"
                 "    progress_data = response['conv-progress'] + '% ';"
                 "    progress_data += '(' + response['current'] + '/' + response['total'] + ') ';"
                 "    progress_data += response['image-file'];"
                 "    $('#progress-data').text(progress_data);"
                 "}")
                target-page))
    `((div (@ (id "progress-container"))
           (h2 ,(_ "Generating thumbnails"))
           (div (@ (class "progress"))
                (div (@ (class "bar")
                        (style "width: 0;"))))
           (div (@ (id "progress-data")))))))

) ;; end module
