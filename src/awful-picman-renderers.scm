(module awful-picman-renderers

  (render-pics
   render-tags
   render-filters
   render-navbar ;; FIXME: should be exported?
   )

(import chicken scheme)
(use data-structures files ports posix srfi-1)
(use awful json slice simple-sha1 uri-common)
(use awful-picman-params
     awful-picman-utils
     awful-picman-db
     awful-picman-image
     awful-picman-ocr)

(define (render-navbar-link href text active?)
  `(li (@ ,(if active?
               '(class "active")
               '()))
       (a (@ (href ,href)) ,text)))

(define (render-navbar active)
  `(nav (@ (class "navbar navbar-default navbar-fixed-top")
           (id "main-navbar"))
        (div (@ (class "container-fluid"))
             (div (@ (class "collapse navbar-collapse"))
                  (ul (@ (class "nav navbar-nav"))
                      ,(render-navbar-link "/albums" (_ "Albums") (eq? active 'albums))
                      ,(render-navbar-link "/folders" (_ "Folders") (eq? active 'folders))
                      ,(render-navbar-link "/tags" (_ "Tags") (eq? active 'tags))
                      (li (@ (class "dropdown"))
                          (a (@ (href "#")
                                (class "dropdown-toggle")
                                (data-toggle "dropdown")
                                (role "button")
                                (aria-expanded "false"))
                             ,(_ "Filters") " " (span (@ (class "caret"))))
                          (ul (@ (class "dropdown-menu")
                                 (role "menu"))
                              (li (a (@ (href "#")) ,(_ "By tags")))
                              (li (a (@ (href "#")) ,(_ "Pics not in albums")))
                              (li (a (@ (href "#")) ,(_ "Pics without tags"))))))
                  ;; (form (@ (class "navbar-form navbar-left")
                  ;;          (role "search"))
                  ;;       (div (@ (class "form-group"))
                  ;;            (input (@ (type "text")
                  ;;                      (class "form-control")
                  ;;                      (placeholder ,(_ "Search")))))
                  ;;       (button (@ (type "submit")
                  ;;                  (class "btn btn-default"))
                  ;;               ,(_ "Submit")))
                  (ul (@ (class "nav navbar-nav navbar-right"))
                      (li (@ (class "dropdown"))
                          (a (@ (href "#")
                                (class "dropdown-toggle")
                                (data-toggle "dropdown")
                                (role "button")
                                (aria-expanded "false"))
                             ,(_ "Batch edit") " " (span (@ (class "caret"))))
                          (ul (@ (class "dropdown-menu")
                                 (role "menu"))
                              (li (a (@ (href "#")) ,(_ "Select all")))
                              (li (a (@ (href "#")) ,(_ "Deselect all")))
                              (li (a (@ (href "#")) ,(_ "Toggle selection")))
                              (li (@ (class "divider")))
                              (li (a (@ (href "#"))
                                     ,(_ "Edit selected thumbnails template"))))))))))

(define (pic-toolbar)
  `(div (@ (id "pic-toolbar"))
        (div (@ (id "prev-pic")
                (class "glyphicon glyphicon-arrow-left")))
        (div (@ (id "next-pic")
                (class "glyphicon glyphicon-arrow-right")))
        (div (@ (id "edit-pic-info")
                (class "glyphicon glyphicon-edit")))
        (div (@ (id "close-zoomed-pic")
                (class "glyphicon glyphicon-remove")))))

(define (pic-info-area)
  `(div (@ (id "pic-info-wrapper"))
        ,(pic-toolbar)
        (div (@ (id "pic-info")))))

(define (zoomed-pic-area)
  `(div (@ (id "zoomed-pic-area-wrapper"))
        (div (@ (id "zoomed-pic-area") ;; FIXME: not needed
                (data-pic-id "null"))
             (div (@ (id "zoomed-pic"))))
        ,(pic-info-area)))

(define (render-thumbnail pic-id pic-path)
  (let* ((pic-id (number->string pic-id))
         (thumbnail-path
          (make-pathname (list (thumbnails-web-dir)
                               (number->string (default-thumbnail-dimension)))
                         pic-path))
         (zoomed-pic-path
          (make-pathname (list (thumbnails-web-dir)
                               (number->string (thumbnails/zoom-dimension)))
                         pic-path)))
    `(img (@ (src ,thumbnail-path)
             (id ,(string-append "pic-" pic-id))
             (tabindex ,pic-id)
             (data-zoomed ,zoomed-pic-path)
             (class "pic-thumbnail")))))


(define (render-dir-stat dir)
  (define (describe count obj)
    (case count
      ((0) (string-append (_ "no ") obj))
      ((1) (string-append "1 " obj))
      (else (sprintf "~a ~as" count obj))))
  (let ((stat (get-dir-stat dir)))
    `(small ,(string-intersperse
              (list (describe (dir-stat-num-pics stat) (_ "pic"))
                    (describe (dir-stat-num-dirs stat) (_ "folder"))
                    (describe (dir-stat-num-files stat) (_ "file")))
              ", "))))

(define (render-folder dir-path)
  (let* ((dirname (pathname-strip-directory dir-path))
         (web-path (make-pathname (folders-web-dir) dir-path)))
    `(div (@ (class "dir"))
          (a (@ (href ,web-path))
             (img (@ (src "/assets/awful-picman/img/dir.png")
                     (alt ,dirname)
                     (class "pic-thumbnail"))))
          (p (a (@ (href ,web-path)) ,dirname))
          ,(render-dir-stat (make-pathname root-dir dir-path)))))

(define (render-other-file-type file-path)
  (let ((filename (pathname-strip-directory file-path)))
    `(div (@ (class "other-file-type pic-thumbnail"))
          (img (@ (src "/assets/awful-picman/img/unknown.png") (alt ,filename)))
          (p ,filename))))

(define (render-thumbnails pics-id/path #!key (folders '()) (other-files '()))
  `(div (@ (id "thumbnails"))
        ,@(append
           (map render-folder folders)
           (map (lambda (id/path)
                  (render-thumbnail (car id/path) (cdr id/path)))
                pics-id/path)
           (map render-other-file-type other-files))))

(define render-filters (lambda args args)) ;; FIXME
(define render-tags (lambda args args)) ;; FIXME

(define (render-pics path-or-album mode . rest) ;; FIXME
  `(,(zoomed-pic-area)
    (div (@ (id "content")) ;; FIXME: move to a more generic place
         ,(case mode
            ((folder)
             (let ((all-files (glob (make-pathname path-or-album "*"))))
               (render-thumbnails (db-get-pics-id/path-by-directory path-or-album)
                                  folders: (filter directory? all-files)
                                  other-files: (remove (lambda (f)
                                                         (or (image-file? f)
                                                             (directory? f)))
                                                       all-files))))
            ((album)
             (if path-or-album
                 (render-thumbnails (db-get-pics-id/path-by-album path-or-album))
                 `(ul (@ (id "albums-list")))))))))

) ;; end module
