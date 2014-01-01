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

(define (render-no-album)
  `(div (@ (id "nothing-here"))
        ,(_ "No album available.  Make albums out of pictures from ")
        (a (@ (href ,(folders-web-dir))) ,(_ "folders"))
        "."))

(define (render-album-content album page-num)
  ;; If album is #f, render all albums
  (debug 1 "render-album-content: album: ~a" album)
  `(,(render-breadcrumbs (or album "/") (_ "Albums") (albums-web-dir))
    ,(if album
         (render-paginated-pics (db-album-pics album) page-num 'album)
         (let ((albums (db-albums)))
           (if (null? albums)
               (render-no-album)
               (render-albums albums))))))
