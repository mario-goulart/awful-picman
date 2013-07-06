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
              (list (describe (dir-stat-num-images stat) "image")
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


(define (render-image-modal dir id image-filename)
  `(div (@ (id ,(string-append "modal-" id))
           (class "modal hide")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby ,image-filename))
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
                                      image-filename))
                               (id ,(string-append "pic-" id)))))
                     (div (@ (class "span2"))
                          (td ,(render-pic-form id)))))))

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
      ,(render-image-modal dir id thumbnail-filename))))

(define (render-pic-form pic-id)
  (define (id thing)
    (list 'id (string-append thing "-" pic-id)))
  `(form
    (fieldset
     (label "Description")
     (textarea (@ ,(id "description")))
     (label "Date")
     (input (@ (type "date") ,(id "date")))
     (br)
     (label "Tags")
     (input (@ (type "text") ,(id "tags")))
     (br)
     (input (@ (type "submit")
               ,(id "submit")
               (class "btn save-pic-info"))))))

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
  (debug "render-directory-content: dir: ~a" dir)

  (define-record item type file idx)

  (define (make-dir path)
    (make-item 'dir (pathname-strip-directory path) 'dummy))

  (define (make-other path)
    (make-item 'other (pathname-strip-directory path) 'dummy))

  (define make-image
    (let ((idx -1))
      (lambda (path)
        (set! idx (+ idx 1))
        (make-item 'image (pathname-strip-directory path) idx))))

  (let* ((items (glob (make-pathname dir "*")))
         (dirs (map make-dir (filter directory? items)))
         (images (map make-image (filter image-file? items)))
         (other (map make-other
                     (remove (lambda (i)
                               (or (directory? i)
                                   (image-file? i)))
                             items)))
         (num-images (length images))
         (dim (default-thumbnail-dimension))
         (rows (chop (append dirs images other) 4)) ;; FIXME: param thumbnails-per-row
         (next-thumb (lambda (current)
                       (let ((idx (item-idx current)))
                         (if (= idx (- num-images 1))
                             #f
                             (item-file (list-ref images (+ idx 1)))))))
         (prev-thumb (lambda (current)
                       (let ((idx (item-idx current)))
                         (if (zero? idx)
                             #f
                             (item-file (list-ref images (- idx 1))))))))
    `(,(render-top-bar)
      ,(render-breadcrumbs dir)
      ,@(map (lambda (row)
               `(div (@ (class "row"))
                     ,@(map (lambda (i)
                              `(div (@ (class "span4"))
                                    ,(case (item-type i)
                                       ((dir) (render-dir-link dir (item-file i)))
                                       ((image) (render-thumbnail dir
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
