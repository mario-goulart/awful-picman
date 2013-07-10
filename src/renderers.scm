(define (render-breadcrumbs path)

  (define home `(li (a (@ (href ,(folders-web-dir))) "Home")))

  (define divider '(li (span (@ (class "divider")) ">")))

  (define (make-path parts)
    (string-intersperse (cons (folders-web-dir) parts) "/"))

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

(define (render-dynamic-input type idx pic-id val)
  `(input (@ (type "text")
             (class ,(sprintf "~a-widget-~a ~a" type pic-id type))
             (id ,(sprintf "~a-~a-~a" type idx pic-id))
             (data-provide "typeahead")
             (value ,val))))

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
        (h5 "Description")
        (p ,(db-pic-descr db-pic))
        (h5 "Date")
        ,(render-date (db-pic-decade db-pic)
                      (db-pic-year db-pic)
                      (db-pic-month db-pic)
                      (db-pic-day db-pic))
        (h5 "Albums")
        ,(if (null? (db-pic-albums db-pic))
             ""
             `(ul ,@(map (lambda (i)
                           `(li ,i))
                         (db-pic-albums db-pic))))
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
         (h5 "Albums")
         ,(render-dynamic-inputs 'album pic-id (db-pic-albums db-pic))
         (h5 "Tags")
         ,(render-dynamic-inputs 'tag pic-id (db-pic-tags db-pic))
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

(define (render-thumbnail thumb-obj dimension prev-filename next-filename)
  (let* ((filename (thumb-filename thumb-obj))
         (id (string->sha1sum filename))
         (dir (thumb-dir thumb-obj))
         (prev-id (and prev-filename (string->sha1sum prev-filename)))
         (next-id (and next-filename (string->sha1sum next-filename))))
    `(;; Link
      (a (@ (href ,(string-append "#modal-" id))
            (data-toggle "modal"))
         (img (@ (class "thumb")
                 (src ,(make-pathname (list (thumbnails-web-dir)
                                            (->string dimension)
                                            dir)
                                      filename)))))
      ;; Modal
      ,(render-pic-modal dir id filename prev-id next-id))))

(define (render-other-file-type filename)
  (let ((size (default-thumbnail-dimension)))
    `(div (@ (class "other-file-type")
             (style ,(sprintf "height: ~a; width: ~a" size size)))
          (img (@ (src "/img/unknown.png") (alt ,filename)))
          (p ,filename))))

(define (create-dynamic-input-ajax type typeahead-source)
  (add-javascript
   (sprintf "
$('.~a').typeahead({
    source: function (query, process) {
        return $.get('~a', { query: query }, function (data) {
            return process(data);
        });
    }
});"
            type typeahead-source))

  (ajax "/add-dynamic_input" (sprintf ".add-~a-widget" type) 'click
        (lambda ()
          (with-request-variables (type pic-id next-idx)
            (render-dynamic-input type next-idx pic-id "")))
        prelude: (string-append
                  (sprintf "var pic_id = $(this).attr('id').replace(/^add-~a-/, '');" type)
                  (sprintf "var next = get_max_dynamic_input_idx('~a', pic_id) + 1;" type))
        arguments: `((pic-id . "pic_id")
                     (type . ,(sprintf "'~a'" type))
                     (next-idx . "next"))
        success: (string-append
                  (sprintf "$(response).insertBefore('#~a-widget-placeholder-' + pic_id);" type)
                  (sprintf "$('#~a-' + next + '-' + pic_id).focus();" type))))


(define (render-album album)
  (let* ((pics
          ($db "select files.path from files, albums where album=? and files.pic_id=albums.pic_id"
               values: (list album)))
         (pic-paths (if (null? pics)
                        '()
                        (map car pics)))
         (thumb-objs
          (map (lambda (pic-path i)
                 (make-thumb 'pic
                             (pathname-directory pic-path)
                             (pathname-strip-directory pic-path)
                             i))
               pic-paths
               (iota (length pic-paths)))))
    (render-thumbnails thumb-objs)))

(define (render-album-content album)
  ;; If album is #f, render all albums
  (debug "render-album-content: album: ~a" album)
  (if album
      (render-album album)
      (let ((albums (db-albums)))
        `(ul ,@(map (lambda (album)
                      (let ((count (db-album-pics-count album)))
                        `(li (a (@ (href ,(string-append "/albums/" album)))
                                ,album)
                             ,(sprintf " (~a ~a)"
                                       count
                                       (if (> count 1)
                                           "pictures"
                                           "picture")))))
                    albums)))))

(define-record thumb type dir filename idx)

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
    `(,(render-breadcrumbs dir)
      ,(render-thumbnails items))))

(define (render-thumbnails items)
  (let ((rows (chop items 4))  ;; FIXME: param thumbnails-per-row
        (dim (default-thumbnail-dimension))
        (num-items (length items)))
    `(div (@ (class "thumbnails-container"))
          ,@(map (lambda (row)
                   `(div (@ (class "row"))
                         ,@(map (lambda (i)
                                  `(div (@ (class "span4")) ;; FIXME: depends on thumbnails-per-row
                                        ,(case (thumb-type i)
                                           ((dir) (render-dir-link i))
                                           ((pic) (render-thumbnail i
                                                                    dim
                                                                    (prev-thumb i items num-items)
                                                                    (next-thumb i items num-items)))
                                           (else (render-other-file-type (thumb-filename i))))))
                                row)))
                 rows))))

(define (render-pics source renderer)
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
}")

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

  (create-dynamic-input-ajax 'tag "/db/tags")
  (create-dynamic-input-ajax 'album "/db/albums")

  (debug "render-pics: source: ~a" source)
  `(,(render-top-bar)
    ,(renderer source)))

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
                  (li (a (@ (href "/albums")) "Albums"))
                  (li (a (@ (class "active") (href "/pics")) "Folders")))
              ,(render-search-form)))))
