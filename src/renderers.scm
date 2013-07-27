(define (render-breadcrumbs path root-label web-root-dir)

  (define (home link?)
    (if link?
        `(li (a (@ (href ,web-root-dir)) ,root-label))
        root-label))

  (define divider '(li (span (@ (class "divider")) ">")))

  (define (make-path parts)
    (string-intersperse (cons web-root-dir parts) "/"))

  (define (link-breadcrumb parts #!key with-divider?)
    `(li (a (@ (href ,(make-path parts))) ,(last parts))
         ,(if with-divider?
              divider
              '())))

  `(ul (@ (class "breadcrumb"))
       ,(if (or (equal? path ".") ;; Is this necessary?
                (equal? path "/"))
            (home #f)
            (let ((parts (string-split path "/")))
              (let loop ((parts parts)
                         (bc '()))
                (if (null? parts)
                    (intersperse (cons (home #t) bc) divider)
                    (loop (butlast parts)
                          (cons (link-breadcrumb parts) bc))))))))

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

(define (render-dir-link dir-obj)
  (let* ((dir (thumb-dir dir-obj))
         (dirname (thumb-filename dir-obj))
         (web-path (make-pathname (list (folders-web-dir) dir)
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

(define (render-dynamic-input type idx pic-id val #!optional prepend-br?)
  `(,(if prepend-br?
         '(br)
         '())
    (input (@ (type "text")
              (class ,(sprintf "~a-widget-~a ~a" type pic-id type))
              (id ,(sprintf "~a-~a-~a" type idx pic-id))
              (data-provide "typeahead")
              (value ,val)))))

(define (render-dynamic-input+ type pic-id)
  `((span (@ (id ,(sprintf "~a-widget-placeholder-~a" type pic-id))))
    (a (@ (href "#")
          (class ,(sprintf "add-~a-widget" type))
          (id ,(sprintf "add-~a-~a" type pic-id)))
       (span (@ (class "badge badge-info"))
             "+"))))

(define (render-dynamic-inputs type pic-id inputs)
  (let* ((len-inputs (length inputs))
         (get-val (lambda (idx)
                    (if (< idx len-inputs)
                        (list-ref inputs idx)
                        ""))))
    `(,(if (zero? len-inputs)
           (render-dynamic-input type 0 pic-id "")
           (intersperse
            (map (lambda (i)
                   (render-dynamic-input type i pic-id (get-val i)))
                 (iota len-inputs))
            '(br)))
      ,(render-dynamic-input+ type pic-id))))

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
    (debug "pic path=~a descr=~a decade=~a year=~a month=~a tags=~S"
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
        (div (@ (class "modal-header"))
             ,(render-modal-toolbar id)
             (button (@ (type "button")
                        (class "close")
                        (data-dismiss "modal")
                        (aria-hidden "true"))
                     ×))
        (div (@ (class "modal-body pic-modal-body"))
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

(define (render-thumbnail thumb-obj dimension prev-filename next-filename mode)
  (let* ((filename (thumb-filename thumb-obj))
         (id (string->sha1sum filename))
         (dir (thumb-dir thumb-obj))
         (prev-id (and prev-filename (string->sha1sum prev-filename)))
         (next-id (and next-filename (string->sha1sum next-filename))))
    `(;; Link
      (a (@ (href ,(string-append "#modal-" id))
            (data-toggle "modal"))
         (img (@ (class "thumb")
                 (id ,(string-append "thumb-" id))
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

(define (render-other-file-type filename)
  (let ((size (default-thumbnail-dimension)))
    `(div (@ (class "other-file-type")
             (style ,(sprintf "height: ~a; width: ~a" size size)))
          (img (@ (src "/img/unknown.png") (alt ,filename)))
          (p ,filename))))

(define (create-dynamic-input-ajax type typeahead-source)

  (define typeahead-source-js
    (sprintf
     "source: function (query, process) {
         return $.get('~a', { query: query }, function (data) {
             return process(data);
         });
     }"
     typeahead-source))

  (add-javascript
   (sprintf "$('.~a').typeahead({~a});"
            type typeahead-source-js))

  (ajax "/add-dynamic-input" (sprintf ".add-~a-widget" type) 'click
        (lambda ()
          (with-request-variables (type pic-id next-idx)
            (render-dynamic-input type next-idx pic-id "" 'prepend-br)))
        prelude: (string-append
                  (sprintf "var pic_id = $(this).attr('id').replace(/^add-~a-/, '');" type)
                  (sprintf "var next = get_max_dynamic_input_idx('~a', pic_id) + 1;" type))
        arguments: `((pic-id . "pic_id")
                     (type . ,(sprintf "'~a'" type))
                     (next-idx . "next"))
        success: (string-append
                  (sprintf "$(response).insertBefore('#~a-widget-placeholder-' + pic_id);" type)
                  (sprintf "$('#~a-' + next + '-' + pic_id).typeahead({~a});"
                           type typeahead-source-js)
                  (sprintf "$('#~a-' + next + '-' + pic_id).focus();" type))))


(define (render-album album)
  (let* ((pic-paths (db-album-pics album))
         (thumb-objs
          (map (lambda (pic-path i)
                 (make-thumb 'pic
                             (pathname-directory pic-path)
                             (pathname-strip-directory pic-path)
                             i))
               pic-paths
               (iota (length pic-paths)))))
    (render-thumbnails thumb-objs 'album)))

(define (render-album-modal album album-id)
  `(div (@ (id ,(conc "album-modal-" album-id))
           (class "modal hide")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby ,(db-album-title album)))
        (div (@ (class "modal-header"))
             (button (@ (type "button")
                        (class "close")
                        (data-dismiss "modal")
                        (aria-hidden "true"))
                     ×)
             (h3 ,(db-album-title album)))
        (div (@ (class "modal-body"))
             (h4 ,(_ "Description"))
             (textarea (@ (id ,(conc "album-descr-" album-id)))
                       ,(db-album-descr album))
             (hr)
             (p ,(_ "Remove album?")
                (literal "&nbsp;")
                (input (@ (type "checkbox")
                          (id ,(conc "album-remove-" album-id)))))
             (br)
             (button (@ (id ,(conc "update-album-info-" album-id))
                        (class "btn update-album-info"))
                     ,(_ "Submit"))
             (button (@ (id "album-cancel")
                        (data-dismiss "modal")
                        (class "btn cancel-save-pic-info"))
                     ,(_ "Cancel")))))


(define (render-album-link album)
  (let* ((title (db-album-title album))
         (descr (db-album-descr album))
         (album-id (db-album-id album))
         (count (db-album-pics-count album-id)))
    (if (> count 0)
        `(li (@ (id ,(conc "album-item-" album-id)))
             (a (@ (href ,(string-append "/albums/" title)))
                ,title)
             ,(sprintf " (~a ~a)"
                       count
                       (if (> count 1)
                           (_ "pictures")
                           (_ "picture")))
             (literal "&nbsp;")
             (span (@ (id ,(conc "album-link-descr-" album-id))
                      (class "album-link-descr"))
                   ,(if (and descr (not (null? descr)))
                        descr
                        ""))
             (literal "&nbsp;")
             (a (@ (href ,(conc "#album-modal-" album-id))
                   (data-toggle "modal"))
                (span (@ (class "icon-edit")))))
        (begin
          (db-remove-album! album-id)
          #f))))

(define (render-albums albums)

  (ajax "/update-album-info" ".update-album-info" 'click
        update-album-info!
        prelude: "var album_id = $(this).attr('id').replace(/^update-album-info-/, '');"
        arguments: `((album-id . "album_id")
                     (remove?  . "$('#album-remove-' + album_id).is(':checked')")
                     (descr    . "$('#album-descr-' + album_id).val()"))
        success: (string-append
                  "if ($('#album-remove-' + album_id).is(':checked'))"
                  "    $('#album-item-' + album_id).remove();"
                  "else"
                  "    $('#album-link-descr-' + album_id).html($('#album-descr-' + album_id).val());"
                  "$('#album-modal-' + album_id).modal('hide');"))

  `(,@(map (lambda (album)
             (let ((album-id (db-album-id album)))
               (render-album-modal album album-id)))
           albums)

    (ul ,@(filter-map render-album-link albums))))

(define (render-album-content album)
  ;; If album is #f, render all albums
  (debug "render-album-content: album: ~a" album)
  `(,(render-breadcrumbs (or album "/") (_ "Albums") (albums-web-dir))
    ,(if album
         (render-album album)
         (let ((albums (db-albums)))
           (if (null? albums)
               (_ "No albums")
               (render-albums albums))))))

(define-record thumb type dir filename idx)

(define-record-printer (thumb obj out)
  (fprintf out "#<thumb type=~a dir=~a filename=~a idx=~a>"
           (thumb-type obj)
           (thumb-dir obj)
           (thumb-filename obj)
           (thumb-idx obj)))

(define (next-thumb current pics num-pics)
  (let ((idx (thumb-idx current)))
    (if (= idx (- num-pics 1))
        #f
        (thumb-filename (list-ref pics (+ idx 1))))))

(define (prev-thumb current pics num-pics)
  (let ((idx (thumb-idx current)))
    (if (zero? idx)
        #f
        (thumb-filename (list-ref pics (- idx 1))))))

(define (render-dir-content dir)

  (define (make-dir path)
    (make-thumb 'dir (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define (make-other path)
    (make-thumb 'other (pathname-directory path) (pathname-strip-directory path) 'dummy))

  (define make-pic
    (let ((idx -1))
      (lambda (path)
        (set! idx (+ idx 1))
        (make-thumb 'pic (pathname-directory path) (pathname-strip-directory path) idx))))

  (let* ((items (glob (make-pathname dir "*")))
         (dirs (map make-dir (filter directory? items)))
         (pics (map make-pic (filter image-file? items)))
         (other (map make-other
                     (remove (lambda (i)
                               (or (directory? i)
                                   (image-file? i)))
                             items)))
         (items (append dirs pics other)))
    `(,(render-breadcrumbs dir (_ "Folders") (folders-web-dir))
      ,(render-thumbnails items 'folder))))

(define (render-thumbnails items mode)
  (let* ((rows (chop items 4))  ;; FIXME: param thumbnails-per-row
         (dim (default-thumbnail-dimension))
         (pics (filter (lambda (item)
                         (eq? (thumb-type item) 'pic))
                       items))
         (num-pics (length pics)))
    `(div (@ (class "thumbnails-container"))
          ,@(map (lambda (row)
                   `(div (@ (class "row"))
                         ,@(map (lambda (i)
                                  `(div (@ (class "span4")) ;; FIXME: depends on thumbnails-per-row
                                        ,(case (thumb-type i)
                                           ((dir) (render-dir-link i))
                                           ((pic)
                                            (render-thumbnail i
                                                              dim
                                                              (prev-thumb i pics num-pics)
                                                              (next-thumb i pics num-pics)
                                                              mode))
                                           (else (render-other-file-type (thumb-filename i))))))
                                row)))
                 rows))))

(define (render-pics source mode)
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

get_max_dynamic_input_idx = function(type, pic_id) {
    return Math.max.apply(Math, $.map($('.' + type + '-widget-' + pic_id), function(i) {
        return i.id.split('-')[1];
    }));
}

get_pic_dynamic_inputs = function(type, pic_id) {
    var elts = $.map($('.' + type + '-widget-' + pic_id), function(i) { return i; });
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
                     (tags   . "JSON.stringify(get_pic_dynamic_inputs('tag', pic_id))")
                     (albums . "JSON.stringify(get_pic_dynamic_inputs('album', pic_id))"))
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
    "tags": JSON.stringify(get_pic_dynamic_inputs('tag', 'pic-template')),
    "albums": JSON.stringify(get_pic_dynamic_inputs('album', 'pic-template')),
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

  (debug "render-pics: source: ~a" source)
  `(,(render-top-bar mode)
    ,(case mode
       ((album) (render-album-content source))
       ((folder) `(,(render-pic-template-modal)
                   ,(render-dir-content source)))
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
  `((div (@ (class "navbar navbar-inverse navbar-fixed-top"))
         (div (@ (class "navbar-inner"))
              (ul (@ (class "nav"))
                  (li (a (@ (href ,(albums-web-dir))) ,(_ "Albums")))
                  (li (a (@ (href ,(folders-web-dir))) ,(_ "Folders"))))
                  (li ,(if (eq? mode 'folder)
                           (render-thumb-toolbar)
                           '())))
         ;; ,(render-search-form) ;; FIXME: not implemented yet
         )))
