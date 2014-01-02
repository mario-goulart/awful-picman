(define (render-modal-toolbar id)
  `((a (@ (href "#")
          (class "zoom-in btn")
          (title ,(_ "Zoom in"))
          (id ,(string-append "zoom-in-" id)))
       (i (@ (class "icon-zoom-in"))))
    (a (@ (href "#")
          (class "zoom-out btn")
          (title ,(_ "Zoom out"))
          (id ,(string-append "zoom-out-" id)))
       (i (@ (class "icon-zoom-out"))))
    (a (@ (href "#")
          (class "prev-pic btn")
          (title ,(_ "Previous picture"))
          (id ,(string-append "btn-prev-pic-" id)))
       (i (@ (class "icon-chevron-left"))))
    (a (@ (href "#")
          (class "next-pic btn")
          (title ,(_ "Next picture"))
          (id ,(string-append "btn-next-pic-" id)))
       (i (@ (class "icon-chevron-right"))))
    (a (@ (href "#")
          (class "btn")
          (title ,(_ "Close"))
          (data-dismiss "modal"))
       (i (@ (class "icon-remove"))))))

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
        (h5 ,(_ "Description"))
        (p ,(db-pic-descr db-pic))
        (h5 ,(_ "Date"))
        ,(render-date (db-pic-decade db-pic)
                      (db-pic-year db-pic)
                      (db-pic-month db-pic)
                      (db-pic-day db-pic))
        (h5 ,(_ "Albums"))
        ,(if (null? (db-pic-albums db-pic))
             ""
             `(ul ,@(map (lambda (i)
                           `(li ,i))
                         (db-pic-albums db-pic))))
        (h5 ,(_ "Tags"))
        ,(if (null? (db-pic-tags db-pic))
             ""
             `(ul ,@(map (lambda (i)
                           `(li ,i))
                         (db-pic-tags db-pic))))
        (h5 ,(_ "Filename"))
        (p (code ,(db-pic-path db-pic)))
        (br)
        (button (@ (class "btn edit-pic-info")
                   ;; Super ugly hack: events don't seem to work with
                   ;; jQuery observers, so we hardcode them here
                   (onclick ,(sprintf "set_pic_info_rw('~a');" pic-id)))
                ,(_ "Edit"))))

(define (render-modal-pic-form/rw pic-id db-pic prev-id next-id #!optional template?)
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
         (h5 ,(_ "Description"))
         (textarea (@ ,(id "descr")) ,(db-pic-descr db-pic))
         (h5 ,(_ "Date (decade/year/month/day)"))
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
         (h5 ,(_ "Albums"))
         ,(render-dynamic-inputs 'album pic-id (db-pic-albums db-pic))
         (h5 ,(_ "Tags"))
         ,(render-dynamic-inputs 'tag pic-id (db-pic-tags db-pic))
         ,(if template?
              '()
              `((h5 ,(_ "Filename"))
                (p (code ,(db-pic-path db-pic)))))
         (br)
         (button (@ ,(id "submit")
                    (class ,(string-append "btn "
                                           (if template?
                                               "batch-save-pic-info"
                                               "save-pic-info"))))
                 ,(_ "Submit"))
         (button (@ ,(id "cancel")
                    ,(if template?
                         '(data-dismiss "modal")
                         `(onclick ,(sprintf "set_pic_info_ro('~a');" pic-id)))
                    (class "btn cancel-save-pic-info"))
                 ,(_ "Cancel")))))

(define (render-modal-pic-form pic-path pic-id prev-id next-id)
  (define (id thing)
    (list 'id (string-append thing "-" pic-id)))
  (let ((db-pic (get-pic-from-db pic-path)))
    (debug 2 "pic path=~a descr=~a decade=~a year=~a month=~a tags=~S"
           pic-path
           (db-pic-descr db-pic)
           (db-pic-decade db-pic)
           (db-pic-year db-pic)
           (db-pic-month db-pic)
           (db-pic-tags db-pic))
    `(,(render-modal-pic-form/ro db-pic pic-id)
      ,(render-modal-pic-form/rw pic-id db-pic prev-id next-id))))

(define (render-pic-modal dir id pic-filename prev-id next-id)
  `(div (@ (id ,(string-append "modal-" id))
           (class "modal hide pic-modal")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby ,pic-filename))
        (div (@ (class "modal-body pic-modal-body"))
             (div (@ (class "row"))
                  (div (@ (class "span9 pic-container"))
                       (img (@ (src ,(make-pathname
                                      (list (thumbnails-web-dir)
                                            (->string (thumbnails/zoom-dimension))
                                            dir)
                                      pic-filename))
                               (id ,(string-append "pic-" id)))))
                     (div (@ (class "span3"))
                          ,(render-modal-toolbar id)
                          ,(render-modal-pic-form (make-pathname dir pic-filename)
                                                  id
                                                  prev-id
                                                  next-id))))))

(define (render-thumbnail thumb-obj dimension prev-filename next-filename mode)
  (let* ((filename (thumb-filename thumb-obj))
         (id (string->sha1sum filename))
         (dir (thumb-dir thumb-obj))
         (prev-id (and prev-filename (string->sha1sum prev-filename)))
         (next-id (and next-filename (string->sha1sum next-filename))))
    `(;; Link
      (a (@ (href ,(string-append "#modal-" id))
            (data-toggle "modal"))
         (img (@ (id ,(string-append "thumb-" id))
                 (src ,(make-pathname (list (thumbnails-web-dir)
                                            (->string dimension)
                                            dir)
                                      filename)))))
      ,(if (eq? mode 'album)
           '()
           `(input (@ (class "thumb-select")
                      (id ,(conc "thumb-select-" id))
                      (type "checkbox"))))
      ;; Modal
      ,(render-pic-modal dir id filename prev-id next-id))))

(define (render-thumbnails items mode)
  (let* ((dim (default-thumbnail-dimension))
         (pics (filter (lambda (item)
                         (eq? (thumb-type item) 'pic))
                       items))
         (num-pics (length pics)))
    `(div (@ (id "thumbnails"))
          ,@(map (lambda (i)
                   `(div (@ (class "thumb"))
                     ,(case (thumb-type i)
                        ((dir) (render-dir-link i))
                        ((pic) (render-thumbnail i
                                                 dim
                                                 (prev-thumb i pics num-pics)
                                                 (next-thumb i pics num-pics)
                                                 mode))
                        (else (render-other-file-type (thumb-filename i))))))
                 items))))

(define (render-pics source mode page-num)
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
")

  (add-dynamic-input-javascript-utils)

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
                     (tags   . "JSON.stringify(get_dynamic_inputs('tag', pic_id))")
                     (albums . "JSON.stringify(get_dynamic_inputs('album', pic_id))"))
        success: (string-append
                  "$('#ro-' + pic_id).html(response);"
                  "set_pic_info_ro(pic_id);"))

  ;; Handle modal for template pic data
  (ajax "/batch-insert-update-pic" ".batch-save-pic-info" 'click
        batch-update-pic-info!
        prelude: #<<EOF
var template_data = {
    "descr": $('#descr-pic-template').val(),
    "decade": $('#decade-pic-template').val(),
    "year": $('#year-pic-template').val(),
    "month": $('#month-pic-template').val(),
    "day": $('#day-pic-template').val(),
    "tags": JSON.stringify(get_dynamic_inputs('tag', 'pic-template')),
    "albums": JSON.stringify(get_dynamic_inputs('album', 'pic-template')),
    };
EOF
        arguments: `((template-data . "JSON.stringify(template_data)")
                     (pics . ,(string-append
                               "JSON.stringify($.map($('.thumb-select'), function(elt, i) {"
                               "    var pic_id = elt.id.replace(/^thumb-select-/, '');"
                               "    if (elt.checked)"
                               "        return [pic_id, $('#thumb-' + pic_id).attr('src')];"
                               "    else"
                               "        return null;"
                               "}))")))
        success: "window.location.reload(true);") ;; FIXME: this is horrible

  (create-dynamic-input-ajax 'tag "/db/tags")
  (create-dynamic-input-ajax 'album "/db/albums")

  (debug 1 "render-pics: source: ~a" source)
  `(,(render-top-bar mode)
    ,(case mode
       ((album) (render-album-content source page-num))
       ((folder) `(,(render-pic-template-modal)
                   ,(render-dir-content source page-num)))
       ((filter/by-tags) (render-filter/by-tags (car source) (cdr source) page-num))
       ((filter/without-album) (render-filter/without-album page-num))
       ((filter/without-tag) (render-filter/without-tag page-num))
       (else (error 'render-pics
                    (sprintf "Unknown mode: ~a" mode))))))

(define (render-search-form)
  `(form (@ (class "navbar-search form-inline pull-right"))
         (input (@ (type "text")
                   (class "input-medium search-query")))
         (button (@ (type "submit")
                    (class "btn-small"))
                 "Search")))


(define (render-pic-template-modal)
  `(div (@ (id "modal-pic-template")
           (class "modal hide")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby "pic-template"))
        (div (@ (class "modal-header"))
             (button (@ (type "button")
                        (class "close")
                        (data-dismiss "modal")
                        (aria-hidden "true"))
                     ×)
             (h4 ,(_ "Template data for selected pictures")))
        (div (@ (class "modal-body"))
             (div (@ (class "alert"))
                  (button (@ (type "button")
                             (class "close")
                             (data-dismiss "alert"))
                          (literal "&times;"))
                  (strong ,(_ "Warning:")) " "
                  ,(_ "All the selected pictures will have their metadata overwritten with the data filled here."))
             ,(let ((pic-template (make-db-pic "pic-template" "" "" "" #f #f #f #f '() '())))
                (render-modal-pic-form/rw "pic-template" pic-template #f #f 'template)))))

(define (render-thumb-toolbar)
  (add-javascript "
$('#thumb-select-all').on('click', function() {
    $('.thumb-select').each(function(i, elt) {
        elt.checked = true;
    });
});

$('#thumb-unselect-all').on('click', function() {
    $('.thumb-select').each(function(i, elt) {
        elt.checked = false;
    });
});

$('#thumb-toggle-selection').on('click', function() {
    $('.thumb-select').each(function(i, elt) {
            elt.checked = !elt.checked;
    });
});

$('#thumb-template').click(function() {
    $('#modal-pic-template').modal('show');
    $('#rw-pic-template').show();
});

$('.dropdown-toggle').dropdown();
")
  (define (render-item id title icon)
    `(li (a (@ (href "#")
               (id ,id)
               (title ,title)
               (class "thumb-toolbar-item presentation"))
            ((i (@ (class ,icon)))
             (literal "&nbsp")
             ,title))))

  `((ul (@ (class "nav pull-right thumb-toolbar"))
        (li (@ (class "dropdown"))
            (a (@ (class "dropdown-toggle")
                  (data-toggle "dropdown")
                  (href "#"))
               ,(_ "Batch edit"))
            (ul (@ (class "dropdown-menu")
                   (role "menu")
                   (aria-labelledby "dLabel"))
                ,(render-item "thumb-select-all"
                              (_ "Select all")
                              "icon-check")
                ,(render-item "thumb-unselect-all"
                              (_ "Unselect all")
                              "icon-share")
                ,(render-item "thumb-toggle-selection"
                              (_ "Toggle selection")
                              "icon-retweet")
                (li (@ (class "divider")))
                ,(render-item "thumb-template"
                              (_ "Edit selected thumbnails template")
                              "icon-edit"))))))

(define (render-top-bar mode)
  (define (item m path text)
    `(li ,(if (eq? m mode)
              `(@ (class "active"))
              '())
         (a (@ (href ,path)) ,text)))
  `((div (@ (class "navbar navbar-inverse navbar-fixed-top"))
         (div (@ (class "navbar-inner"))
              (ul (@ (class "nav"))
                  ,(item 'album (albums-web-dir) (_ "Albums"))
                  ,(item 'folder (folders-web-dir) (_ "Folders"))
                  ,(item 'tag (tags-web-dir) (_ "Tags"))
                  ,(render-filters-menu mode))
              ,(if (eq? mode 'folder)
                   (render-thumb-toolbar)
                   '()))
         ;; ,(render-search-form) ;; FIXME: not implemented yet
         )))

(define (render-pagination-links num-total-items current-page #!optional (more-url-vars/vals '()))
  (let ((num-pages (inexact->exact
                    (ceiling (/ num-total-items (thumbnails/page)))))
        (link-page (lambda (page)
                     (string-append "?"
                                    (form-urlencode
                                     (cons `(page . ,page)
                                           more-url-vars/vals))))))
    (if (< num-pages 2)
        '()
        `(div (@ (class "pagination pagination-centered"))
              (ul
               ,@(map (lambda (i)
                        (let ((current-page? (= i current-page)))
                          `(li ,(if current-page?
                                    `(@ (class "active"))
                                    '())
                               (a (@ (href ,(if current-page?
                                                "#"
                                                (link-page i))))
                                  ,(+ i 1)))))
                      (iota num-pages)))))))


(define (render-paginated-pics pic-paths page-num mode #!key (url-vars/vals '()))

  (define (paginate pic-paths page-num)
    (let* ((offset (* page-num (thumbnails/page)))
           (page-pic-paths
            (slice pic-paths offset (+ offset (thumbnails/page)))))
      (map (lambda (pic-path i)
             (make-thumb 'pic
                         (pathname-directory pic-path)
                         (pathname-strip-directory pic-path)
                         i))
           page-pic-paths
           (iota (length page-pic-paths)))))

  (let ((num-pics (length pic-paths))
        (thumb-objs (paginate pic-paths page-num)))
    `(,(render-thumbnails thumb-objs mode)
      ,(render-pagination-links num-pics page-num url-vars/vals))))
