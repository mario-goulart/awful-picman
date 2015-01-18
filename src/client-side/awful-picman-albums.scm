;;;
;;; Albums
;;;
(define album-infos-being-edited '())

(define (album-info-being-edited? album-id)
  (and (member album-id album-infos-being-edited) #t))

(define (album-info-add-to-edited! album-id)
  (unless (album-info-being-edited? album-id)
    (set! album-infos-being-edited
          (cons album-id album-infos-being-edited))))

(define (album-info-remove-from-edited! album-id)
  (set! album-infos-being-edited
        (delete album-id album-infos-being-edited)))

(define (render-album-title/ro album-id title)
  `(a (@ (href ,(string-append "/albums/" title)))
      (span (@ (id ,(conc "album-title-" album-id))) ,title)))

(define (render-album-title/rw album-id title)
  `(input (@ (type "text")
             (id ,(string-append "album-title-" album-id))
             (value ,title))))

(define (render-album-info album)
  (let* ((title (alist-ref 'title album))
         (descr (alist-ref 'description album))
         (album-id (alist-ref 'id album))
         (num-pics (alist-ref 'num-pics album)))
    (and (> num-pics 0)
         `(li (@ (id ,(conc "album-" album-id)))
              (span (@ (id ,(conc "album-title-wrapper-" album-id)))
                    ,(render-album-title/ro album-id title))
              ,(conc " (" num-pics " "
                     (if (> num-pics 1)
                         (_ "pictures")
                         (_ "picture")) ")")
              (span (@ (id ,(conc "album-descr-" album-id)))
                    ,descr)
              (span (@ (data-album-id ,album-id)
                       (id ,(conc "edit-album-info-" album-id))
                       (class "edit-album-info glyphicon glyphicon-edit"))
                    "") ;; FIXME: spock needs this or it will nest spans.  Bug?
              (span (@ (data-album-id ,album-id)
                       (class "remove-album glyphicon glyphicon-remove"))
                    "") ;; FIXME: spock needs this or it will nest spans.  Bug?
              (span (@ (data-album-id ,album-id)
                       (id ,(conc "save-album-info-" album-id))
                       (style "display: none;")
                       (class "save-album-info glyphicon glyphicon-ok"))
                    ""))))) ;; FIXME: spock needs this or it will nest spans.  Bug?

(define (render-no-album)
  `(div (@ (id "nothing-here"))
        ,(_ "No album available.  Make albums out of pictures from ")
        (a (@ (href ,(folders-web-dir))) ,(_ "folders"))
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

(define (set-album-info-editable! album-id)
  (album-info-add-to-edited! album-id)
  (let ((album-title (jtext ($ (string-append "#album-title-" album-id)))))
    (jhtml! ($ (string-append "#album-title-wrapper-" album-id))
            (sxml->html (render-album-title/rw album-id album-title)))
    (shade-icon ($ (string-append "#edit-album-info-" album-id)))
    (jshow ($ (string-append "#save-album-info-" album-id)))))

(define (set-album-info-read-only! album-id)
  (debug (conc "set-album-info-read-only!: album-id: " album-id))
  (album-info-remove-from-edited! album-id)
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
         (album-title (jval ($ (string-append "#album-title-" album-id))))
         (album-description "")) ;; FIXME
    (debug (conc "save-album-info: title: " album-title))
    (remote-write (string-append "/write-album-info/" album-id)
                  `((id . ,(string->number album-id))
                    (title . ,album-title)
                    (description . ,album-description)))
    (set-album-info-read-only! album-id)))

(define (remove-album! event)
  (album-info-remove-from-edited! album-id)
  (debug "remove-album!"))

;;;
;;; Event handlers
;;;
(live-on ($ "#content") "click"  ".edit-album-info"
         (lambda (event)
           (let* ((this (jcurrent-target event))
                  (album-id (jattr this "data-album-id")))
             (if (album-info-being-edited? album-id)
                 (set-album-info-read-only! album-id)
                 (set-album-info-editable! album-id)))))

(live-on ($ "#content") "click" ".remove-album" remove-album!)

(live-on ($ "#content") "click"  ".save-album-info" save-album-info)


;;;
;;; Render albums
;;;
(render-albums ($ "#albums-list"))
