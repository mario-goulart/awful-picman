;;;
;;; Albums
;;;

(read-conf-from-server!)

(define (render-album-info album)
  (let* ((title (alist-ref 'title album))
         (descr (alist-ref 'description album))
         (album-id (alist-ref 'id album))
         (num-pics (alist-ref 'num-pics album)))
    (and (> num-pics 0)
         `(li (@ (id ,(conc "album-" album-id)))
              (a (@ (href ,(string-append "/albums/" title)))
                 (span (@ (id ,(conc "album-title-" album-id))) ,title))
              ,(conc " (" num-pics " "
                     (if (> num-pics 1)
                         (_ "pictures")
                         (_ "picture")) ")")
              (span (@ (id ,(conc "album-descr-" album-id)))
                    ,descr)
              (span (@ (data-album-id ,album-id)
                       (title ,(_ "Edit"))
                       (class "edit-album-info album-button-bar glyphicon glyphicon-edit"))
                    "") ;; FIXME: spock needs this or it will nest spans.  Bug?
              (span (@ (data-album-id ,album-id)
                       (title ,(_ "Export"))
                       (class "export-album album-button-bar glyphicon glyphicon-share"))
                    "") ;; FIXME: spock needs this or it will nest spans.  Bug?
              ))))

(define (render-no-album)
  `(div (@ (id "no-album"))
        ,(_ "No album available.  Make albums out of pictures from ")
        (a (@ (href "/folders")) ,(_ "folders")) ;; FIXME: hardcoded /folders
        "."))

(define (render-albums albums-list-elt)
  (remote-read "/albums-info"
               (lambda (albums)
                 (jhtml! albums-list-elt
                         (sxml->html
                          (if (null? albums)
                              (render-no-album)
                              `(ul
                                ,@(filter-map render-album-info albums))))))))

(define (set-album-info-read-only! album-id)
  (debug (conc "set-album-info-read-only!: album-id: " album-id))
  (unshade-icon ($ (string-append "#edit-album-info-" album-id)))
  (jhide ($ (string-append "#save-album-info-" album-id)))
  (remote-read (string-append "/read-album-info/" album-id)
               (lambda (album-info)
                 (jhtml! ($ (string-append "#album-title-wrapper-" album-id))
                         (sxml->html
                          (render-album-title/ro album-id
                                                 (alist-ref 'title album-info)))))))

(define (save-album-info event)
  (let* ((this (jcurrent-target event))
         (album-id (jattr this "data-album-id"))
         (album-title (jval ($ "#album-new-title")))
         (album-description (jval ($ "#album-new-description"))))
    (debug (conc "save-album-info: title: " album-title))
    (if (jis ($ "#album-remove") ":checked")
        (remote-write (conc "/remove-album/" album-id) "")
        (remote-write (string-append "/write-album-info/" album-id)
                      `((id . ,(string->number album-id))
                        (title . ,album-title)
                        (description . ,album-description)))))
  (hide-modal ($ "#album-edit-modal"))
  (jattr! ($ "#album-remove") "checked" #f)
  (render-albums ($ "#albums-list"))) ;; FIXME: update only altered albums?

(define (edit-album-info event)
  (let* ((this (jcurrent-target event))
         (album-id (jattr this "data-album-id")))
    (remote-read (string-append "/read-album-info/" album-id)
                 (lambda (album-info)
                   (jattr! ($ "#save-album-info") "data-album-id" album-id)
                   (jval! ($ "#album-new-title")
                          (alist-ref 'title album-info))
                   (jval! ($ "#album-new-description")
                          (alist-ref 'description album-info))
                   (show-modal ($ "#album-edit-modal"))))))

(define (popup-export-album-modal event)
  (let* ((this (jcurrent-target event))
         (album-id (jattr this "data-album-id"))
         (album-title (jtext ($ (string-append "#album-title-" album-id)))))
    (jtext! ($ "#album-export-title") album-title)
    (show-modal ($ "#album-export-modal"))))

(define (export-album)
  (remote-read (string-append "/export-album/"
                              (jtext ($ "#album-export-title"))
                              "?dir=" (jval ($ "#album-export-dir")))
               (lambda (__)
                 (hide-modal ($ "#album-export-modal")))))


;;;
;;; Event handlers
;;;

(on ($ "#save-album-info") "click" save-album-info)

(live-on ($ "#content") "click" ".edit-album-info" edit-album-info)

(live-on ($ "#content") "click" ".export-album" popup-export-album-modal)

(on ($ "#export-album") "click" export-album)


;;;
;;; Render albums
;;;
(render-albums ($ "#albums-list"))
