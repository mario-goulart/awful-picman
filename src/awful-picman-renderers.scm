(module awful-picman-renderers

  (render-pics
   render-tags
   render-filters
   render-navbar ;; FIXME: should be exported?
   render-breadcrumbs ;; FIXME: should be exported?  Make a pic & album page render procedure
   )

(import chicken scheme)
(use data-structures extras files ports posix srfi-1)
(use awful json slice uri-common)
(use awful-picman-params
     awful-picman-utils
     awful-picman-db
     awful-picman-image
     awful-picman-ocr)

(include "renderers/tag.scm")
(include "renderers/breadcrumbs.scm")
(include "renderers/filter.scm")

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
                              (li (a (@ (href ,(make-absolute-pathname (filters-web-dir)
                                                                       "by-date")))
                                     ,(_ "By date")))
                              (li (a (@ (href ,(make-absolute-pathname (filters-web-dir)
                                                                       "by-tags")))
                                     ,(_ "By tags")))
                              (li (a (@ (href ,(make-absolute-pathname (filters-web-dir)
                                                                       "without-tag")))
                                     ,(_ "Pics without tags")))
                              (li (a (@ (href ,(make-absolute-pathname (filters-web-dir)
                                                                       "not-in-albums")))
                                     ,(_ "Pics not in albums")))
                              )))
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
                             ,(_ "Pics/page") " " (span (@ (class "caret"))))
                          (ul (@ (class "dropdown-menu")
                                 (role "menu"))
                              ,@(map
                                 (lambda (thumbs/page)
                                   `(li
                                     (@ ,(if (= thumbs/page (thumbnails/page))
                                             '(class "disabled")
                                             '()))
                                     (a (@ (href
                                            ,(string-append
                                              "/set-thumbnails-per-page?"
                                              (form-urlencode
                                               `((thumbs-per-page . ,thumbs/page)
                                                 (go-back-to . ,(update-url-query-string
                                                                 `((pagenum . 0)))))))))
                                        ,thumbs/page)))
                                 (thumbnails/page-steps))))
                      (li (@ (class "dropdown"))
                          (a (@ (href "#")
                                (class "dropdown-toggle")
                                (data-toggle "dropdown")
                                (role "button")
                                (aria-expanded "false"))
                             ,(_ "Batch edit") " " (span (@ (class "caret"))))
                          (ul (@ (class "dropdown-menu")
                                 (role "menu"))
                              (li (a (@ (href "#")
                                        (id "select-all"))
                                     ,(_ "Select all")))
                              (li (a (@ (href "#")
                                        (id "deselect-all"))
                                     ,(_ "Deselect all")))
                              (li (a (@ (href "#")
                                        (id "toggle-selection"))
                                     ,(_ "Toggle selection")))
                              (li (a (@ (href "#")
                                        (id "select-from-to"))
                                     ,(_ "Select from/to")))
                              (li (@ (class "divider")))
                              (li (a (@ (href "#")
                                        (id "batch-edit"))
                                     ,(_ "Edit selected thumbnails template"))))))))))


(define (render-pager num-pics pagenum)
  (let* ((num-pages (inexact->exact
                     (ceiling (/ num-pics (thumbnails/page)))))
         (link-page (lambda (pagenum)
                      (update-url-query-string `((pagenum . ,pagenum))))))
    (if (< num-pages 2)
        '()
        `(div (@ (id "pager")
                 (class "text-center"))
              (ul (@ (class "pagination pagination-centered"))
                  ,@(map (lambda (i)
                           (let ((current-page? (= i pagenum)))
                             `(li ,(if current-page?
                                       `(@ (class "active"))
                                       '())
                                  (a (@ (href ,(if current-page?
                                                   "#"
                                                        (link-page i))))
                                          ,(+ i 1)))))
                              (iota num-pages)))))))

(define (pic-toolbar)
  `(div (@ (id "pic-toolbar"))
        (div (@ (id "prev-pic")
                (title ,(_ "Previous"))
                (class "glyphicon glyphicon-arrow-left")))
        (div (@ (id "next-pic")
                (title ,(_ "Next"))
                (class "glyphicon glyphicon-arrow-right")))
        (div (@ (id "rotate-pic")
                (title ,(_ "Rotate 90"))
                (class "glyphicon glyphicon-repeat")))
        (div (@ (id "edit-pic-info")
                (title ,(_ "Edit"))
                (class "glyphicon glyphicon-edit")))
        (div (@ (id "close-zoomed-pic")
                (title ,(_ "Close"))
                (class "glyphicon glyphicon-remove")))))

(define (pic-info-area)
  `(div (@ (id "pic-info-wrapper"))
        ,(pic-toolbar)
        (div (@ (id "pic-info")))))

(define (zoomed-pic-area)
  `(div (@ (id "zoomed-pic-area-wrapper"))
        (div (@ (id "zoomed-pic")))
        ,(pic-info-area)))

(define (render-thumbnail pic-id pic-path)
  (let* ((pic-id (number->string pic-id))
         (thumbnail-path
          (normalize-pathname
           (make-pathname (list (thumbnails-web-dir)
                                (number->string (thumbnails/small-dimension)))
                          pic-path)))
         (zoomed-pic-path
          (normalize-pathname
           (make-pathname (list (thumbnails-web-dir)
                                (number->string (thumbnails/zoom-dimension)))
                          pic-path))))
    `(li
      ((img (@ (src ,thumbnail-path)
               (id ,(string-append "pic-" pic-id))
               (tabindex ,pic-id)
               (data-zoomed ,zoomed-pic-path)
               (width ,(thumbnails/small-dimension))
               (class "pic-thumbnail")))
       (span (@ (class "pic-select-container"))
             (input (@ (class "pic-select")
                       (data-pic-id ,pic-id)
                       (type "checkbox"))))))))


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
    `(li
      (div (@ (class "dir"))
           (a (@ (href ,web-path))
              (img (@ (src "/assets/awful-picman/img/dir.png")
                      (alt ,dirname)
                      (class "pic-thumbnail"))))
           (p (a (@ (href ,web-path)) ,dirname))
           ,(render-dir-stat (make-pathname root-dir dir-path))))))

(define (render-video-file file-path)
  (let ((filename (pathname-strip-directory file-path)))
    `(li
      (div (@ (class "video-file-type pic-thumbnail"))
           (a (@ (href ,(make-absolute-pathname #f file-path)))
              (img (@ (src "/assets/awful-picman/img/video.png")
                      (alt ,filename))))
           (p ,filename)))))

(define (render-other-file-type file-path)
  (let ((filename (pathname-strip-directory file-path)))
    `(li
      (div (@ (class "other-file-type pic-thumbnail"))
           (img (@ (src "/assets/awful-picman/img/unknown.png") (alt ,filename)))
           (p ,filename)))))

(define (render-thumbnails pics-id/path pagenum
                           #!key (folders '())
                                 (video-files '())
                                 (other-files '()))
  (let* ((pics-slice (* (thumbnails/page) pagenum))
         (items-to-show
          (slice pics-id/path
                 pics-slice
                 (+ pics-slice (thumbnails/page)))))
    `((div (@ (id "thumbnails"))
           (ul (@ (class "thumbnail-list"))
               ,@(append
                  (map render-folder (sort folders string<?))
                  (map (lambda (id/path)
                         (render-thumbnail (car id/path) (cdr id/path)))
                       (sort items-to-show
                             (lambda (p1 p2)
                               (string<? (cdr p1) (cdr p2)))))
                  (map render-video-file video-files)
                  (map render-other-file-type other-files))))
      ,(render-pager (length pics-id/path) pagenum))))

(define (render-modal id #!key (title "") (body '()) (footer '()))
  `(div (@ (class "modal fade")
           (id ,id))
        (div (@ (class "modal-dialog"))
             (div (@ (class "modal-content"))
                  (div (@ (class "modal-header"))
                       (button (@ (type "button")
                                  (class "close")
                                  (data-dismiss "modal")
                                  (aria-label "Close"))
                               (span (@ (aria-hidden "true"))
                                     (literal "&times;")))
                       (h4 (@ (class "modal-title"))
                           ,title))
                  (div (@ (class "modal-body"))
                       ,body)
                  (div (@ (class "modal-footer"))
                       ,footer)))))

(define (render-album-edit-modal)
  (render-modal "album-edit-modal"
                title: (_ "Edit album")
                body: `((h4 ,(_ "Title"))
                        (input (@ (type "text")
                                  (id "album-new-title")
                                  (value "")))
                        (h4 ,(_ "Description"))
                        (textarea (@ (id "album-new-description"))
                                  "")
                        (hr)
                        (p ,(_ "Remove album?")
                           (literal "&nbsp;")
                           (input (@ (type "checkbox")
                                     (id "album-remove")))))
                footer: `((button (@ (type "button")
                                     (data-dismiss "modal")
                                     (class "btn btn-default"))
                                  ,(_ "Cancel"))
                          (button (@ (id "save-album-info")
                                     (data-album-id "")
                                     (type "button")
                                     (class "btn btn-primary"))
                                  ,(_ "Submit")))))

(define (render-album-export-modal)
  (render-modal "album-export-modal"
                title: (_ "Export album")))

(define (render-pic-template-modal)
  (render-modal "pic-template-modal"
                title: (_ "Template data for selected pictures")
                body: `((div (@ (id "pic-template-form-container"))))
                footer: `((button (@ (data-dismiss "modal")
                                     (type "button")
                                     (class "btn btn-default"))
                                  ,(_ "Cancel"))
                          (button (@ (id "save-pic-template")
                                     (type "button")
                                     (class "btn btn-primary"))
                                  ,(_ "Save")))))

(define (render-albums)
  `(,(render-album-edit-modal)
    ,(render-album-export-modal)
    (div (@ (id "albums-list")))))


(define (render-pics mode #!key pagenum
                                (with-zoomed-area? #t)
                                path
                                album-id
                                tags
                                start-date
                                end-date)
  (let ((pagenum (or pagenum 0)))
    `(,(if with-zoomed-area? (zoomed-pic-area) '())
      (div (@ (id "content")) ;; FIXME: move to a more generic place
           ,(render-pic-template-modal)
           ,(case mode
              ((folder)
               (let ((all-files (glob (make-pathname path "*"))))
                 (render-thumbnails (db-get-pics-id/path-by-directory path)
                                    pagenum
                                    folders: (filter directory? all-files)
                                    video-files: (filter video-file? all-files)
                                    other-files: (remove (lambda (f)
                                                           (or (image-file? f)
                                                               (video-file? f)
                                                               (directory? f)))
                                                         all-files))))
              ((album)
               (if album-id
                   (render-thumbnails (db-get-pics-id/path-by-album-id album-id)
                                      pagenum)
                   (render-albums)))

              ((filter/by-tags)
               (let ((include-tags (car tags))
                     (exclude-tags (cdr tags)))
                 (render-filter/by-tags (db-tag-filter include-tags exclude-tags)
                                        include-tags
                                        exclude-tags
                                        pagenum)))

              ((filter/by-date)
               (render-filter/by-date
                (if (and end-date (date-decade end-date))
                    (db-get-pics-id/path-by-date-range start-date end-date)
                    '())
                start-date
                end-date
                pagenum))

              ((filter/without-tag)
               (render-filter/without-tag (db-filter/without-tag) pagenum))

              ((filter/not-in-albums)
               (render-filter/not-in-albums (db-filter/not-in-albums) pagenum))

              )))))

) ;; end module
