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

(define (report-progress progress-file total current)
  (with-output-to-file progress-file
    (lambda ()
      ;; show progress in percentage
      (print (flonum->fixnum (/ (* current 100) total))))))

(define (images->thumbnails* imgs/thumbs/dims progress-file)
  (let ((num-thumbs (length imgs/thumbs/dims)))
    (for-each (lambda (img/thumb/dim i)
                (let ((image-file (car img/thumb/dim))
                      (thumbnail (cadr img/thumb/dim))
                      (dimension (caddr img/thumb/dim)))
                  (when progress-file
                    (report-progress progress-file num-thumbs i))
                  (image->thumbnail image-file thumbnail dimension)))
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
          (if (file-read-access? progress-file)
              (let ((percent (with-input-from-file progress-file read-line)))
                `((conv-progress . ,(if (string->number percent)
                                        (conc percent "%")
                                        ""))
                  (status . running)))
              `((conv-progress . "")
                (status . done)))))
      update-targets: #t
      arguments: `((progress-file . ,(sprintf "'~a'" progress-file)))
      success: (sprintf
                (string-append
                 "var progress;"
                 "if (response['status'] == 'done') {"
                 "    window.location.replace('~a')"
                 "} else {"
                 "    progress = response['conv-progress'];"
                 "    if (progress) {"
                 "        $('.bar').width(progress);"
                 "        $('#progress-percent').text(progress);"
                 "    }"
                 "};")
                target-page))
    `((div (@ (id "progress-container"))
           (h2 ,(_ "Generating thumbnails"))
           (div (@ (class "progress"))
                (div (@ (class "bar")
                        (style "width: 0;"))))
           (div (@ (id "progress-percent")))))))
