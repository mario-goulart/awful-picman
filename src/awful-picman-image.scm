(module awful-picman-image

  (thumbnail-path
   maybe-replace-thumbnail-extension
   image->thumbnail
   rotate-image!
   )

(import chicken scheme)
(use data-structures extras files ports posix srfi-1 srfi-4)
(use awful exif imlib2)
(use awful-picman-params awful-picman-utils)

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

(define (rotate-image! pic-path)
  (for-each (lambda (dir)
              (let* ((thumb-path (thumbnail-path pic-path dir))
                     (image (image-load thumb-path)))
                (debug 2 "rotate-image!: rotating file ~a" thumb-path)
                (image-save (image-orientate image 1) ;; rotate 90
                            thumb-path)
                (image-destroy image)))
            (list (thumbnails/small-dimension)
                  (thumbnails/zoom-dimension))))

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

(define (image->thumbnail/exif image-file thumb-path)
  ;; If image-file contains thumbnail data, write it to thumb-path and
  ;; return #t.  Otherwise, return #f.
  (let ((thumb-data (thumbnail->u8vector image-file)))
    (and thumb-data
         (begin
           (debug 2 "Writing exif thumbnail data from ~a to ~a"
                  image-file thumb-path)
           (with-output-to-file thumb-path
             (lambda ()
               (write-u8vector thumb-data)))
           #t))))

(define (image->thumbnail/imlib2 image-file thumb-path dimension)
  (let ((image (image-load image-file))
        (non-web? (non-web-image-file? image-file)))
    ;; Only resize image if it is bigger than the max thumbnail
    ;; dimension
    (if (or (> (image-width image) dimension)
            (> (image-height image) dimension)
            non-web?)
        (begin
          (info* "Generating thumbnail for ~a (dimension=~a)"
                 image-file dimension)
          (when non-web?
            (image-format-set! image (thumbnails/default-extension)))
          (handle-exceptions exn
            (info-error "image->thumbnail: error when generating thumbnail for ~a"
                        image-file)
            (image-save (image-scale/proportional image dimension)
                        thumb-path)))
        (file-copy image-file
                   (maybe-replace-thumbnail-extension thumb-path)
                   'clobber))
    (image-destroy image)))

(define (image->thumbnail image-file dimension)
  (let ((thumb-path (thumbnail-path image-file dimension)))
    (debug 2 "image->thumbnail: thumbnail: ~a" thumb-path)
    (create-directory (pathname-directory thumb-path) 'with-parents)
    (if (= dimension thumbnails/exif-dimension)
        (unless (image->thumbnail/exif image-file thumb-path)
          (debug 2 "Failed to extrac exif thumbnail data from ~a" image-file)
          (image->thumbnail/imlib2 image-file thumb-path dimension))
        (image->thumbnail/imlib2 image-file thumb-path dimension))))

) ;; end module
