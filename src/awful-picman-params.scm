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
 _

 ;; Misc
 verbose?
 start-decade
 debug-level
 debug-formatter

 ;; Constants
 dot-dirname
 thumbnails-dirname
 db-filename
 assets-install-dir
 root-dir
 metadata-dir
 global-conf-dir

 ;; Image
 image-file-extensions
 non-web-image-file-extensions

 ;; OCR
 ocr-supported-formats
 ocr-program
 ocr-args
 ocr-languages
 ocr-default-language
)

(import chicken scheme)
(use data-structures extras files posix setup-api srfi-4 utils)
(use hostinfo spiffy)
(use (only awful page-access-control))
(reexport (only awful page-access-control))

;;;
;;; Access control
;;;
(page-access-control
 (lambda (dummy)
   (equal? "localhost"
           (ip->hostname
            (list->u8vector
             (map string->number
                  (string-split (remote-address) ".")))))))

;;;
;;; Web path-related parameters
;;;
(define folders-web-dir (make-parameter "/folders"))
(define thumbnails-web-dir (make-parameter "/thumbnails"))
(define albums-web-dir (make-parameter "/albums"))
(define tags-web-dir (make-parameter "/tags"))
(define filters-web-dir (make-parameter "/filter"))

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
  (make-parameter 50))

;;;
;;; i18n
;;;
(define language
  ;; if not set, awful-picman will try to use LANG and LC_ALL
  ;; environment variables
  (make-parameter #f))

;; _ is actually a procedure which is set as soon as the program runs
(define _)

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


;;;
;;; Constants
;;;
(define dot-dirname ".awful-picman")
(define thumbnails-dirname "thumbnails")
(define db-filename "awful-picman.db")

;; Where chicken-install will install static files served by the web
;; server.  This stuff will be copied to the metadata dir on --init.
(define assets-install-dir
  (make-pathname (list (installation-prefix)
                       "lib"
                       "chicken"
                       (number->string (##sys#fudge 42)))
                 "awful-picman"))

(define root-dir #f) ;; set as soon as the program starts running

(define metadata-dir
  (make-pathname "." dot-dirname))

(define global-conf-dir
  (make-pathname (get-environment-variable "HOME") dot-dirname))

;;;
;;; Image stuff
;;;
(define image-file-extensions
  '("png" "jpg" "jpeg" "gif" "tiff" "tif"))

(define non-web-image-file-extensions
  '("tiff" "tif"))


;;;
;;; OCR stuff
;;;
(define ocr-supported-formats
  (make-parameter '("tif" "tiff")))

(define ocr-program
  (make-parameter "tesseract"))

(define ocr-args
  (make-parameter
   (lambda (pic-file text-file lang)
     (sprintf "~a ~a -l ~a"
              (qs pic-file)
              (qs text-file)
              lang))))

(define ocr-languages
  ;; tesseract doesn't provide any easy way to list the supported
  ;; languages, so we list some of them here (it's possible that not
  ;; all of them are installed!)
  (make-parameter
   `((eng . "English")
     (deu . "German")
     (fra . "French")
     (ita . "Italian")
     (por . "Portuguese")
     (spa . "Spanish")
     )))

(define ocr-default-language
  ;; awful picman will try to guess the default OCR language based on
  ;; the i18n language
  (make-parameter #f))

) ;; end module
