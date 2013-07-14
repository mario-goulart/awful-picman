(module awful-picman-params

;;; Exported symbols
(;; Web path-related parameters
 folders-web-dir thumbnails-web-dir albums-web-dir

 ;; Image-related parameters
 thumbnails/max-dimensions
 thumbnails/zoom-dimension
 default-thumbnail-extension

 ;; i18n
 language

 ;; Misc
 verbose?
 start-decade
)

(import chicken scheme)

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
;;; i18n
;;;
(define language
  ;; if not set, awful-picman will try to use LANG and LC_ALL
  ;; environment variables
  (make-parameter #f))

;;;
;;; Assorted parameters
;;;
(define verbose? (make-parameter #f))
(define start-decade (make-parameter 1900))

) ;; end module
