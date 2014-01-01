(module awful-picman-params

;;; Exported symbols
(;; Web path-related parameters
 folders-web-dir
 thumbnails-web-dir
 albums-web-dir
 tags-web-dir
 filters-web-dir

 ;; Image-related parameters
 thumbnails/max-dimensions
 thumbnails/zoom-dimension
 thumbnails/default-extension
 thumbnails/page

 ;; i18n
 language

 ;; Misc
 verbose?
 start-decade
 debug-level
 debug-formatter
)

(import chicken scheme)
(use extras)

;;;
;;; Web path-related parameters
;;;
(define folders-web-dir (make-parameter "/folders"))
(define thumbnails-web-dir (make-parameter "/thumbnails"))
(define albums-web-dir (make-parameter "/albums"))
(define tags-web-dir (make-parameter "/tags"))
(define filters-web-dir (make-parameter "/filters"))

;;;
;;; Image-related parameters
;;;
(define thumbnails/max-dimensions (make-parameter '(300)))
(define thumbnails/zoom-dimension (make-parameter 1700))
(define thumbnails/default-extension
  ;; For when converting non-web image files to web image files
  (make-parameter "jpg"))

(define thumbnails/page
  ;; Max number of thumbnails per page.
  (make-parameter 30))

;;;
;;; i18n
;;;
(define language
  ;; if not set, awful-picman will try to use LANG and LC_ALL
  ;; environment variables
  (make-parameter #f))

;;;
;;; Messages & debugging
;;;
(define verbose? (make-parameter #f))

(define debug-level (make-parameter 0))

(define debug-formatter
  (make-parameter
   (lambda (level fmt)
     (sprintf "DEBUG[~a] ~a\n" level fmt))))

;;;
;;; Assorted parameters
;;;
(define start-decade (make-parameter 1900))

) ;; end module
