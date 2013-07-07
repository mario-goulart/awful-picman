(define (render-breadcrumbs path)

  (define home `(li (a (@ (href ,(pics-web-dir))) "Home")))

  (define divider '(li (span (@ (class "divider")) ">")))

  (define (make-path parts)
    (string-intersperse (cons (pics-web-dir) parts) "/"))

  (define (link-breadcrumb parts #!key with-divider?)
    `(li (a (@ (href ,(make-path parts))) ,(last parts))
         ,(if with-divider?
              divider
              '())))

  `(ul (@ (class "breadcrumb"))
       ,(if (equal? path ".")
            home
            (let loop ((parts (string-split path "/"))
                       (bc '()))
              (if (null? parts)
                  (intersperse (cons home bc) divider)
                  (loop (butlast parts)
                        (cons (link-breadcrumb parts) bc)))))))


(define (render-dir-stat dir)
  (define (describe count obj)
    (case count
      ((0) (string-append "no " obj))
      ((1) (string-append "1 " obj))
      (else (sprintf "~a ~as" count obj))))
  (let ((stat (get-dir-stat dir)))
    `(small ,(string-intersperse
              (list (describe (dir-stat-num-pics stat) "pic")
                    (describe (dir-stat-num-dirs stat) "folder")
                    (describe (dir-stat-num-files stat) "file"))
              ", "))))

(define (render-dir-link dir dirname)
  (let ((web-path (make-pathname (list (pics-web-dir) dir)
                                 dirname))
        (size (default-thumbnail-dimension)))
    `(div (@ (class "dir")
             (style ,(sprintf "height: ~a; width: ~a" size size)))
          (a (@ (href ,web-path))
             (img (@ (src "/img/dir.png") (alt ,dirname))))
          (p (a (@ (href ,web-path)) ,dirname))
          ,(render-dir-stat (make-pathname (list root-dir dir) dirname)))))

(define (render-modal-toolbar id)
  `((a (@ (href "#")
          (class "zoom-in btn")
          (id ,(string-append "zoom-in-" id)))
       (i (@ (class "icon-zoom-in"))))
    (a (@ (href "#")
          (class "zoom-out btn")
          (id ,(string-append "zoom-out-" id)))
       (i (@ (class "icon-zoom-out"))))))


(define (render-modal-pic-form pic-path pic-id)
  (define (id thing)
    (list 'id (string-append thing "-" pic-id)))
  (let ((db-pic (get-pic-from-db pic-path)))
    (debug "pic path=~a descr=~a decade=~a year=~a month=~a tags=~S"
           pic-path
           (db-pic-descr db-pic)
           (db-pic-decade db-pic)
           (db-pic-year db-pic)
           (db-pic-month db-pic)
           (db-pic-tags db-pic))
    `(div
      (fieldset
       (label "Description")
       (textarea (@ ,(id "descr")) ,(db-pic-descr db-pic))
       (label "Date (decade/year/month/day)")
       ,(combo-box (string-append "decade-" pic-id)
                   (iota (+ 1 (/ (- (current-decade) (start-decade)) 10))
                         (start-decade)
                         10)
                   default: (db-pic-decade db-pic)
                   class: "input-4digits")
       ,(combo-box (string-append "year-" pic-id) (iota 10)
                   default: (db-pic-year db-pic)
                   class: "input-1digit")
       ,(combo-box (string-append "month-" pic-id) (iota 12 1)
                   month: (db-pic-month db-pic)
                   class: "input-2digits")
       ,(combo-box (string-append "day-" pic-id) (iota 31 1)
                   default: (db-pic-day db-pic)
                   class: "input-2digits")
       (br)
       (label "Tags")
       (input (@ (type "text") ,(id "tags"))
              ,(string-intersperse (db-pic-tags db-pic) ", "))
       (br)
       (input (@ (type "hidden") ,(id "path") (value ,pic-path)))
       (input (@ (type "submit")
                 ,(id "submit")
                 (class "btn save-pic-info")))))))

(define (render-pic-modal dir id pic-filename)
  `(div (@ (id ,(string-append "modal-" id))
           (class "modal hide")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby ,pic-filename))
        (div (@ (class "modal-header"))
             ,(render-modal-toolbar id)
             (button (@ (type "button")
                        (class "close")
                        (data-dismiss "modal")
                        (aria-hidden "true"))
                     ×))

        (div (@ (class "modal-body"))
             (div (@ (class "row"))
                  (div (@ (class "span12 pic-container"))
                       (img (@ (src ,(make-pathname
                                      (list (thumbnails-web-dir)
                                            (->string (thumbnails/zoom-dimension))
                                            dir)
                                      pic-filename))
                               (id ,(string-append "pic-" id)))))
                     (div (@ (class "span2"))
                          (td
                           ,(render-modal-pic-form (make-pathname dir pic-filename) id)))))))

(define (render-thumbnail dir thumbnail-filename dimension prev next)
  (let ((id (string->sha1sum thumbnail-filename)))
    `(;; Link
      (a (@ (href ,(string-append "#modal-" id))
            (data-toggle "modal"))
         (img (@ (class "thumb")
                 (src ,(make-pathname (list (thumbnails-web-dir)
                                            (->string dimension)
                                            dir)
                                      thumbnail-filename)))))
      ;; Modal
      ,(render-pic-modal dir id thumbnail-filename))))

(define (render-other-file-type filename)
  (let ((size (default-thumbnail-dimension)))
    `(div (@ (class "other-file-type")
             (style ,(sprintf "height: ~a; width: ~a" size size)))
          (img (@ (src "/img/unknown.png") (alt ,filename)))
          (p ,filename))))

(define (render-directory-content dir)
  (add-javascript
   "
$('.zoom-in').on('click', function() {
   var img = $('#pic-' + $(this).attr('id').replace(/^zoom-in-/, ''));
   img.css('width', parseInt(img.css('width').replace(/px$/, '')) + 15);
});

$('.zoom-out').on('click', function() {
   var img = $('#pic-' + $(this).attr('id').replace(/^zoom-out-/, ''));
   img.css('width', parseInt(img.css('width').replace(/px$/, '')) - 15);
});
")

  ;; Handle the modal pic form
  (ajax "/insert-update-pic" ".save-pic-info" 'click
        update-pic-info!
        prelude: "var pic_id = $(this).attr('id').replace(/^submit-/, '');"
        arguments: `((path   . "$('#path-' + pic_id).val()")
                     (descr  . "$('#descr-' + pic_id).val()")
                     (decade . "$('#decade-' + pic_id).val()")
                     (year   . "$('#year-' + pic_id).val()")
                     (month  . "$('#month-' + pic_id).val()")
                     (day    . "$('#day-' + pic_id).val()")
                     (tags   . "$('#tags-' + pic_id).val()")))

  (debug "render-directory-content: dir: ~a" dir)

  (define-record item type file idx)

  (define (make-dir path)
    (make-item 'dir (pathname-strip-directory path) 'dummy))

  (define (make-other path)
    (make-item 'other (pathname-strip-directory path) 'dummy))

  (define make-pic
    (let ((idx -1))
      (lambda (path)
        (set! idx (+ idx 1))
        (make-item 'pic (pathname-strip-directory path) idx))))

  (let* ((items (glob (make-pathname dir "*")))
         (dirs (map make-dir (filter directory? items)))
         (pics (map make-pic (filter image-file? items)))
         (other (map make-other
                     (remove (lambda (i)
                               (or (directory? i)
                                   (image-file? i)))
                             items)))
         (num-pics (length pics))
         (dim (default-thumbnail-dimension))
         (rows (chop (append dirs pics other) 4)) ;; FIXME: param thumbnails-per-row
         (next-thumb (lambda (current)
                       (let ((idx (item-idx current)))
                         (if (= idx (- num-pics 1))
                             #f
                             (item-file (list-ref pics (+ idx 1)))))))
         (prev-thumb (lambda (current)
                       (let ((idx (item-idx current)))
                         (if (zero? idx)
                             #f
                             (item-file (list-ref pics (- idx 1))))))))
    `(,(render-top-bar)
      ,(render-breadcrumbs dir)
      ,@(map (lambda (row)
               `(div (@ (class "row"))
                     ,@(map (lambda (i)
                              `(div (@ (class "span4")) ;; FIXME: depends on thumbnails-per-row
                                    ,(case (item-type i)
                                       ((dir) (render-dir-link dir (item-file i)))
                                       ((pic) (render-thumbnail dir
                                                                (item-file i)
                                                                dim
                                                                (prev-thumb i)
                                                                (next-thumb i)))
                                       (else (render-other-file-type (item-file i))))))
                            row)))
             rows))))

(define (render-search-form)
  `(form (@ (class "navbar-search form-inline pull-right"))
         (input (@ (type "text")
                   (class "input-medium search-query")))
         (button (@ (type "submit")
                    (class "btn-small"))
                 "Search")))

(define (render-top-bar)
  `((div (@ (class "navbar navbar-inverse navbar-fixed-top"))
         (div (@ (class "navbar-inner"))
              (ul (@ (class "nav"))
                  (li (a (@ (class "active") (href "/pics")) "By folder"))
                  (li (a (@ (href "/by-year")) "By year"))
                  (li (a (@ (href "/by-tag")) "By tag")))
              ,(render-search-form)))))
