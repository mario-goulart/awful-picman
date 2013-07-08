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
       (i (@ (class "icon-zoom-out"))))
    (a (@ (href "#")
          (class "prev-pic btn")
          (id ,(string-append "btn-prev-pic-" id)))
       (i (@ (class "icon-chevron-left"))))
    (a (@ (href "#")
          (class "next-pic btn")
          (id ,(string-append "btn-next-pic-" id)))
       (i (@ (class "icon-chevron-right"))))))

(define (render-date decade year month day)
  (cond ((and decade year month day)
         (sprintf "~a-~a-~a" (+ decade year) month day))
        ((and decade year month)
         (sprintf "~a-~a" (+ decade year) month))
        ((and decade year)
         (sprintf "~a" (+ decade year)))
        (decade decade)
        (else "")))

(define (render-modal-pic-form/ro db-pic pic-id)
  (define (id thing)
    (list 'id (string-append thing "-" pic-id)))
  `(div (@ ,(id "ro")
           (class "pic-info-ro"))
        (h5 "Description")
        (p ,(db-pic-descr db-pic))
        (h5 "Date")
        ,(render-date (db-pic-decade db-pic)
                      (db-pic-year db-pic)
                      (db-pic-month db-pic)
                      (db-pic-day db-pic))
        (h5 "Tags")
        ,(if (null? (db-pic-tags db-pic))
             ""
             `(ul ,@(map (lambda (i)
                           `(li ,i))
                         (db-pic-tags db-pic))))
        (h5 "Filename")
        (p (code ,(db-pic-path db-pic)))
        (br)
        (button (@ (class "btn edit-pic-info")
                   ;; Super ugly hack: events don't seem to work with
                   ;; jQuery observers, so we hardcode them here
                   (onclick ,(sprintf "set_pic_info_rw('~a');" pic-id)))
                "Edit")))

(define (render-tag-widget idx pic-id val)
  `(input (@ (type "text")
             (class ,(sprintf "tag-widget-~a" pic-id))
             (id ,(sprintf "tag-~a-~a" idx pic-id))
             (data-provide "typeahead")
             (value ,val))))

(define (render-tag+ pic-id)
  `((span (@ (id ,(sprintf "tag-widget-placeholder-~a" pic-id))))
    (a (@ (href "#")
          (class "add-tag-widget")
          (id ,(sprintf "add-tag-~a" pic-id)))
       (span (@ (class "badge badge-info"))
             "+"))))

(define (render-tag-widgets/rw pic-id tags)
  (let* ((len-tags (length tags))
         (get-tag (lambda (idx)
                    (if (< idx len-tags)
                        (list-ref tags idx)
                        ""))))
    `(,(if (zero? len-tags)
           (render-tag-widget 0 pic-id "")
           (intersperse
            (map (lambda (i)
                   (render-tag-widget i pic-id (get-tag i)))
                 (iota len-tags))
            '(br)))
      ,(render-tag+ pic-id))))

(define (render-modal-pic-form/rw pic-id db-pic pic-id prev-id next-id)
  (define (id thing)
    (list 'id (string-append thing "-" pic-id)))
  `(div (@ ,(id "rw")
           (class "pic-info-rw"))
        (fieldset
         (input (@ (type "hidden")
                   ,(id "prev-pic")
                   (value ,(or prev-id ""))))
         (input (@ (type "hidden")
                   ,(id "next-pic")
                   (value ,(or next-id ""))))
         (input (@ (type "hidden")
                   ,(id "path")
                   (value ,(db-pic-path db-pic))))
         (h5 "Description")
         (textarea (@ ,(id "descr")) ,(db-pic-descr db-pic))
         (h5 "Date (decade/year/month/day)")
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
         (h5 "Tags")
         (div (@ (id ,(sprintf "tags-container-~a" pic-id)))
              ,(render-tag-widgets/rw pic-id (db-pic-tags db-pic)))
         (h5 "Filename")
         (p (code ,(db-pic-path db-pic)))
         (br)
         (button (@ ,(id "submit")
                    (class "btn save-pic-info"))
                 "Submit")
         (button (@ ,(id "cancel")
                    ;; Another ugly hack
                    (onclick ,(sprintf "set_pic_info_ro('~a');" pic-id))
                    (class "btn cancel-save-pic-info"))
                 "Cancel"))))

(define (render-modal-pic-form pic-path pic-id prev-id next-id)
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
    `(,(render-modal-pic-form/ro db-pic pic-id)
      ,(render-modal-pic-form/rw pic-id db-pic pic-id prev-id next-id))))

(define (render-pic-modal dir id pic-filename prev-id next-id)
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
                     (div (@ (class "span4"))
                          (td
                           ,(render-modal-pic-form (make-pathname dir pic-filename)
                                                   id
                                                   prev-id
                                                   next-id)))))))

(define (render-thumbnail dir thumbnail-filename dimension prev-filename next-filename)
  (let ((id (string->sha1sum thumbnail-filename))
        (prev-id (and prev-filename (string->sha1sum prev-filename)))
        (next-id (and next-filename (string->sha1sum next-filename))))
    `(;; Link
      (a (@ (href ,(string-append "#modal-" id))
            (data-toggle "modal"))
         (img (@ (class "thumb")
                 (src ,(make-pathname (list (thumbnails-web-dir)
                                            (->string dimension)
                                            dir)
                                      thumbnail-filename)))))
      ;; Modal
      ,(render-pic-modal dir id thumbnail-filename prev-id next-id))))

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

$('.next-pic').on('click', function() {
   var id = $(this).attr('id').replace(/^btn-next-pic-/, '');
   var next = $('#next-pic-' + id).val();
   $('#modal-' + id).modal('hide');
   if(next) {
       $('#modal-' + next).modal('show');
   }
});

$('.prev-pic').on('click', function() {
   var id = $(this).attr('id').replace(/^btn-prev-pic-/, '');
   var prev = $('#prev-pic-' + id).val();
   $('#modal-' + id).modal('hide');
   if(prev) {
       $('#modal-' + prev).modal('show');
   }
});

$('.tags').typeahead({
    source: function (query, process) {
        return $.get('/db/tags', { query: query }, function (data) {
            return process(data);
        });
    }
});

get_max_tag_idx = function(pic_id) {
    return Math.max.apply(Math, $.map($('.tag-widget-' + pic_id), function(i) {
        return i.id.split('-')[1];
    }));
}

get_pic_tags = function(pic_id) {
    var elts = $.map($('.tag-widget-' + pic_id), function(i) { return i; });
    return $.map(elts, function(i) { return $(i).val(); });
}

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
                     (id     . "pic_id")
                     (tags   . "JSON.stringify(get_pic_tags(pic_id))"))
        success: (string-append
                  "$('#ro-' + pic_id).html(response);"
                  "set_pic_info_ro(pic_id);"))

  (ajax "/add-tag" ".add-tag-widget" 'click
        add-tag-widget
        prelude: (string-append
                  "var pic_id = $(this).attr('id').replace(/^add-tag-/, '');"
                  "var next_tag = get_max_tag_idx(pic_id) + 1;")
        arguments: `((pic-id . "pic_id")
                     (next-idx . "next_tag"))
        success: (string-append
                  "$(response).insertBefore('#tag-widget-placeholder-' + pic_id);"
                  "$('#tag-' + next_tag + '-' + pic_id).focus();"))

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
      (div (@ (class "dir-content"))
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
                  rows)))))

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
