(define image-file-extensions
  '("png" "jpg" "jpeg" "gif"))

(define (image-file? file)
  (let ((extension (pathname-extension file)))
    (and extension
         (member (string-downcase extension) image-file-extensions))))

(define (flonum->fixnum num)
  (inexact->exact (round num)))

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

(define (image->thumbnail image-file dimension)
  (let ((thumbnail (make-pathname (list metadata-dir
                                        thumbnails-dirname
                                        (->string dimension))
                                  image-file)))
    (debug "image->thumbnail: thumbnail: ~a" thumbnail)
    (debug "image->thumbnail: image-file: ~a" image-file)

    ;; Don't create thumbnail if it already exists
    (unless (file-exists? thumbnail)
      (let ((image (image-load image-file)))
        ;; Only resize image if it is bigger than the max thumbnail
        ;; dimension
        (if (or (> (image-width image) dimension)
                (> (image-height image) dimension))
            (begin
              (create-directory (pathname-directory thumbnail) 'with-parents)
              (image-save (image-scale/proportional image dimension)
                          thumbnail))
            (file-copy image-file thumbnail 'clobber))
        (image-destroy image)))))
