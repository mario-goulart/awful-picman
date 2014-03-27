(module awful-picman-image

  (default-thumbnail-dimension
   thumbnail-path
   maybe-replace-thumbnail-extension
   images->thumbnails
   )

(import chicken scheme)
(use data-structures files ports posix srfi-1 extras)
(use imlib2 awful)
(use awful-picman-params awful-picman-utils)

(define (default-thumbnail-dimension)
  (car (thumbnails/max-dimensions)))

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
  (let ((image (image-load image-file))
        (non-web? (non-web-image-file? image-file)))
    ;; Only resize image if it is bigger than the max thumbnail
    ;; dimension
    (if (or (> (image-width image) dimension)
            (> (image-height image) dimension)
            non-web?)
        (begin
          (info* "Generating thumbnail for ~a (dimension=~a)" image-file dimension)
          (when non-web?
            (image-format-set! image (thumbnails/default-extension)))
          (handle-exceptions exn
            (info-error "image->thumbnail: error when generating thumbnail for ~a" image-file)
            (image-save (image-scale/proportional image dimension)
                        thumbnail)))
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


) ;; end module
