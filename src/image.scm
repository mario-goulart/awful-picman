(define (default-thumbnail-dimension)
  (car (thumbnails/max-dimensions)))

(define image-file-extensions
  '("png" "jpg" "jpeg" "gif" "tiff"))

(define non-web-image-file-extensions
  '("tiff"))

(define (image-file? file)
  (let ((extension (pathname-extension file)))
    (and extension
         (member (string-downcase extension) image-file-extensions)
         #t)))

(define (non-web-image-file? file)
  (let ((extension (pathname-extension file)))
    (and extension
         (member (string-downcase extension)
                 non-web-image-file-extensions)
         #t)))

(define (flonum->fixnum num)
  (inexact->exact (round num)))

(define (thumbnail-path pic-path dimension)
  (normalize-pathname
   (make-pathname (list metadata-dir
                        thumbnails-dirname
                        (->string dimension))
                  (maybe-replace-thumbnail-extension pic-path))))

(define (maybe-replace-thumbnail-extension thumbnail)
  (if (non-web-image-file? thumbnail)
      (pathname-replace-extension thumbnail
                                  (thumbnails/default-extension))
      thumbnail))

(define (image-scale/proportional image max-dimension)
  ;; Scale the given image keeping width/height proportion and
  ;; limiting the new values to `max-dimension'.
  (let* ((w (image-width image))
         (h (image-height image))
         (w-proportion (/ w max-dimension))
         (h-proportion (/ h max-dimension))
         (scale-factor (if (> h-proportion w-proportion)
                           h-proportion
                           w-proportion)))
    (image-scale image
                 (flonum->fixnum (/ w scale-factor))
                 (flonum->fixnum (/ h scale-factor)))))

(define (image->thumbnail image-file thumbnail dimension)
  (debug 2 "image->thumbnail: thumbnail: ~a" thumbnail)
  (create-directory (pathname-directory thumbnail) 'with-parents)
  (let ((image (image-load image-file)))
    ;; Only resize image if it is bigger than the max thumbnail
    ;; dimension
    (if (or (> (image-width image) dimension)
            (> (image-height image) dimension))
        (begin
          (info* "Generating thumbnail for ~a (dimension=~a)" image-file dimension)
          (when (non-web-image-file? image-file)
            (image-format-set! image (thumbnails/default-extension)))
          (image-save (image-scale/proportional image dimension)
                      thumbnail))
        (file-copy image-file
                   (maybe-replace-thumbnail-extension thumbnail)
                   'clobber))
    (image-destroy image)))

(define (report-progress progress-file image-file dimension total current)
  (with-output-to-file progress-file
    (lambda ()
      (write (list image-file dimension total current)))))

(define (images->thumbnails* imgs/thumbs/dims progress-file)
  (let ((num-thumbs (length imgs/thumbs/dims)))
    (for-each (lambda (img/thumb/dim i)
                (let ((image-file (car img/thumb/dim))
                      (thumbnail (cadr img/thumb/dim))
                      (dimension (caddr img/thumb/dim)))
                  (when progress-file
                    (report-progress progress-file image-file dimension num-thumbs i))
                  (image->thumbnail image-file thumbnail dimension)
                  ;; alleviate gc pressure. Without this explicit call
                  ;; to gc, memory consumpion easily reaches 1GB when
                  ;; converting a few hundreds of picures.
                  (gc)))
              imgs/thumbs/dims
              (iota num-thumbs 1))))

(define (images->thumbnails imgs/thumbs/dims #!optional fork?)
  ;; imgs/thumbs/dims => '((image-file thumbnail-file dimension) ...)
  (if fork?
      (let ((progress-file (create-temporary-file)))
        (process-fork
         (lambda ()
           (images->thumbnails* imgs/thumbs/dims progress-file)
           (delete-file* progress-file)))
        progress-file)
      (images->thumbnails* imgs/thumbs/dims #f)))

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
