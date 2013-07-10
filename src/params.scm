;;;
;;; Web path-related parameters
;;;
(define folders-web-dir (make-parameter "/folders"))
(define thumbnails-web-dir (make-parameter "/thumbnails"))
(define albums-web-dir (make-parameter "/albums"))

;;;
;;; Image-related parameters
;;;
(define thumbnails/max-dimensions (make-parameter '(300)))
(define thumbnails/zoom-dimension (make-parameter 1700))
(define default-thumbnail-extension
  ;; For when converting non-web image files to web image files
  (make-parameter "jpg"))


;;;
;;; Assorted parameters
;;;
(define verbose? (make-parameter #f))
(define start-decade (make-parameter 1900))
