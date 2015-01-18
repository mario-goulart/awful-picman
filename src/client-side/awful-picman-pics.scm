(define thumbnails/max-dimensions #f)
(define thumbnails/zoom-dimension #f)

(remote-read "/conf"
             (lambda (data)
               (debug "Configuration data from server:")
               (debug data)
               (debug (alist-ref 'i18n-language data))
               (set! thumbnails/max-dimension (alist-ref 'thumbnails/max-dimension data))
               (set! thumbnails/zoom-dimension (alist-ref 'thumbnails/zoom-dimension data))
               (set! i18n-language
                     (case (alist-ref 'i18n-language data)
                       ((en) i18n/en)
                       ((pt-br) i18n/pt-br)
                       (else #f)))))

(define (scale-pic pic)
  ;; pic is a DOM element
  (let* ((maxw (%host-ref "$(window).width()"))
         (maxh (%host-ref "$(window).height()"))
         (w (.width pic))
         (h (.height pic))
         (r (/ h w)))
    (debug (conc "maxw=" maxw ", maxh=" maxh ", w=" w ", h=" h))
    (when (> h maxh)
      (set! h maxh)
      (set! w (/ h r)))
    (when (> w maxw)
      (set! w maxw)
      (set! h (* w r)))
    (set! w (- (%inline "Math.floor" w) 1))
    (set! h (- (%inline "Math.floor" h) 1))
    (debug (conc "Scaling pic to " w "x" h))
    (set! (.width pic) w)
    (set! (.height pic) h)
    pic))

(define (render-date decade year month day)
  (cond ((and decade year month day)
         (conc (+ decade year) "/" month "/" day " (yyyy/mm/dd)"))
        ((and decade year month)
         (conc (+ decade year) "/" month " (yyyy/mm)"))
        ((and decade year)
         (conc (+ decade year) " (yyyy)"))
        (decade
         (conc decade " (" (_ "decade") ")"))
        (else "")))

(define (make-pic-data-getter pic-data-alist)
  (lambda (field . default)
    (let ((val (alist-ref field pic-data-alist)))
      (or val
          (if (null? default)
              #f
              default)))))

(define pic-info-edit-mode? #f)

(define (enter-pic-info-edit-mode!)
  (set! pic-info-edit-mode? #t)
  (shade-icon ($ "#edit-pic-info")))

(define (leave-pic-info-edit-mode!)
  (set! pic-info-edit-mode? #f)
  (unshade-icon ($ "#edit-pic-info")))

(define (render-pic-info)
  (remote-read (string-append "/read-pic-info/" (get-pic-id))
               (lambda (pic-data)
                 (leave-pic-info-edit-mode!)
                 (jhtml! ($ "#pic-info")
                         (sxml->html
                          (let* ((pic-data (make-pic-data-getter pic-data)))
                            `(div (@ (id "pic-form"))
                                  (h3 ,(_ "Description"))
                                  (p (@ (id "pic-description-wrapper"))
                                     ,(pic-data 'description ""))
                                  (h3 ,(_ "Date"))
                                  (p (@ (id "pic-date"))
                                     ,(let ((date (pic-data 'date)))
                                        (if date
                                            (apply render-date date)
                                            "")))
                                  (h3 ,(_ "Tags"))
                                  (div (@ (id "pic-tags"))
                                       ,(itemize (pic-data 'tags '())))
                                  (h3 ,(_ "Albuns"))
                                  (div (@ (id "pic-albums"))
                                       ,(itemize (pic-data 'albums '())))
                                  (div (@ (id "pic-info-edit-button-bar")
                                          (style "visibility: hidden;"))
                                       (button (@ (id "cancel-edit-pic-info"))
                                               "Cancel")
                                       (button (@ (id "save-pic-info"))
                                               "Save")))))))))

(define (render-typeahead-input class val)
  `(div (@ (class "remove-typeahead"))
        (input (@ (type "text")
                  (class ,class)
                  (value ,val)))
        (span (@ (class "remove-typeahead-icon glyphicon glyphicon-minus")))))

(define (render-typeahead-inputs class items)
  `(div
    ,(itemize
      (if (null? items)
          (list (render-typeahead-input class ""))
          (map (lambda (item)
                 (render-typeahead-input class item))
               items)))
    (span (@ (class "add-typeahead-icon glyphicon glyphicon-plus")
             (data-class ,class)))))

(define (set-pic-info-editable!)
  (enter-pic-info-edit-mode!)
  (remote-read (string-append "/read-pic-info/" (get-pic-id))
    (lambda (pic-data)
      (let* ((pic-data (make-pic-data-getter pic-data))
             (tags (pic-data 'tags '()))
             (albums (pic-data 'albums '())))
        (jhtml! ($ "#pic-description-wrapper")
                (sxml->html
                 `(textarea (@ (id "pic-description")
                               (rows 1))
                            ,(pic-data 'description ""))))
        (jhtml! ($ "#pic-date")
                (let* ((date (pic-data 'date))
                       (decade (if date (car date) ""))
                       (year (if date (cadr date) ""))
                       (month (if date (caddr date) ""))
                       (day (if date (cadddr date) "")))
                  (sxml->html
                   `(,(combo-box "pic-date-decade" (iota 10 1920 10) #t decade "decade")
                     ,(combo-box "pic-date-year" (iota 10) #t year "year")
                     ,(combo-box "pic-date-month" (iota 12 1) #t month "month")
                     ,(combo-box "pic-date-day" (iota 31 1) #t day "day")))))
        (jhtml! ($ "#pic-tags")
                (sxml->html (render-typeahead-inputs "tag-typeahead" tags)))
        (jhtml! ($ "#pic-albums")
                (sxml->html (render-typeahead-inputs "album-typeahead" albums)))
        (jfocus ($ "#pic-description"))
        (setup-typeahead-listener!))))
  (%inline .css ($ "#pic-info-edit-button-bar") "visibility" "visible"))

(define (get-pic-id)
  (substring (jattr ($ "#zoomed-pic img") "data-pic-id") 4))

(define (show-zoomed-pic)
  (let ((zoomed-pic-area-wrapper ($ "#zoomed-pic-area-wrapper"))
        (zoomed-pic-id (jattr ($ "#zoomed-pic img") "data-pic-id")))
    (%inline .css zoomed-pic-area-wrapper
             "top" (%host-ref "$(document).scrollTop()") "px;")
    (render-pic-info)
    (jshow zoomed-pic-area-wrapper)
    (let ((window-width (%host-ref "$(window).width()"))
          (pic-width (%inline .width ($ "#zoomed-pic img")))
          (pic-height (%inline .height ($ "#zoomed-pic img"))))
      (debug (conc "window-width: " window-width ", pic-width: " pic-width))
      (debug (conc "pic-info-area width: " (- window-width pic-width)))
      (%inline .css ($ "#pic-info-wrapper")
               (% "max-width" (- window-width pic-width)
                  "min-height" pic-height))
      (%inline .css ($ "#pic-info") (% "height" pic-height))
      (unshade-icon ($ "#edit-pic-info"))
      (%inline .addClass ($ "body") "modal-open")
      (jshow ($ "#pic-info-wrapper")))))

(define-native loadImage)

(define (load-image src id)
  (debug (conc "Loading image. src=" src ", id=" id))
  (loadImage src
             (callback
              (lambda (img)
                (jattr! ($ img) "data-pic-id" id)
                (jhtml! ($ "#zoomed-pic") (scale-pic img))
                (show-zoomed-pic)))))

(define (current-pic-index pics)
  (let* ((pic-id (string-append "#" (jattr ($ "#zoomed-pic img") "data-pic-id")))
         (current-pic ($ pic-id)))
    (%inline .index pics current-pic)))

(define (prev-pic event)
  (let* ((pics ($ ".pic-thumbnail"))
         (idx (current-pic-index pics)))
    (if (> idx 0)
        (let ((prev-pic (vector-ref pics (- idx 1))))
          (unshade-icon ($ "#prev-pic"))
          (unshade-icon ($ "#next-pic"))
          (load-image (jattr ($ prev-pic) "data-zoomed") (.id prev-pic)))
        (shade-icon ($ "#prev-pic")))
    #f))

(define (next-pic event)
  (let* ((pics ($ ".pic-thumbnail"))
         (idx (current-pic-index pics)))
    (if (< idx (- (.length pics) 1))
        (let ((next-pic (vector-ref pics (+ idx 1))))
          (unshade-icon ($ "#next-pic"))
          (unshade-icon ($ "#prev-pic"))
          (load-image (jattr ($ next-pic) "data-zoomed") (.id next-pic)))
        (shade-icon ($ "#next-pic")))
    #f))

(define (close-zoomed-pic)
  (leave-pic-info-edit-mode!)
  (let ((pic-id (jattr ($ "#zoomed-pic img") "data-pic-id")))
    (jhide ($ "#zoomed-pic-area-wrapper"))
    (%inline .removeClass ($ "body") "modal-open")
    (debug (string-append "focusing " pic-id))
    (jfocus ($ (string-append "#" pic-id)))
    #f))

(define (save-pic-info)
  (let ((pic-id (get-pic-id))
        (description (jval ($ "#pic-description")))
        (decade (jval ($ "#pic-date-decade")))
        (year (jval ($ "#pic-date-year")))
        (month (jval ($ "#pic-date-month")))
        (day (jval ($ "#pic-date-day")))
        (tags (filter-map (lambda (elt)
                            (let ((val (.value elt)))
                              (and (not (equal? val ""))
                                   val)))
                          (vector->list (%inline .toArray ($ ".tag-typeahead")))))
        (albums (filter-map (lambda (elt)
                              (let ((val (.value elt)))
                                (and (not (equal? val ""))
                                     val)))
                            (vector->list (%inline .toArray ($ ".album-typeahead"))))))
    (remote-write (string-append "/write-pic-info/" pic-id)
                  `((id . ,(string->number pic-id))
                    (description . ,description)
                    (date ,@(map string->number (list decade year month day)))
                    (tags . ,tags)
                    (albums . ,albums)))
    (remote-read (string-append "/read-pic-info/" pic-id)
                 (lambda (pic-data)
                   (jhtml! ($ "#pic-info")
                           (sxml->html
                            (render-pic-info pic-data)))))
    (unshade-icon ($ "#edit-pic-info"))
    (debug "saving pic info")))

;;; Thumbnails
(define (zoom-pic target)
  (load-image (jattr target "data-zoomed")
              (jattr target "id"))
  #f)

(define (current-thumbnail)
  (let* ((focused (.activeElement (%host-ref "document")))
         (jfocused ($ focused)))
    (and (jhas-class? jfocused "pic-thumbnail")
         jfocused)))

(define (current-thumbnail-index thumbnails)
  (let ((current (current-thumbnail)))
    (and current
         (%inline .index thumbnails current))))

(define (prev-thumbnail event)
  (let* ((pics ($ ".pic-thumbnail"))
         (idx (current-thumbnail-index pics)))
    (when (and idx (> idx 0))
      (let ((prev-pic (vector-ref pics (- idx 1))))
        (jfocus ($ (string-append "#" (.id prev-pic))))))))

(define (next-thumbnail event)
  (let* ((pics ($ ".pic-thumbnail"))
         (idx (current-thumbnail-index pics)))
    (when (and idx (< idx (- (.length pics) 1)))
      (let ((next-pic (vector-ref pics (+ idx 1))))
        (jfocus ($ (string-append "#" (.id next-pic))))))))

;;;
;;; Init stuff
;;;
(jhide ($ "#zoomed-pic-area-wrapper"))


;;;
;;; Event handlers
;;;
(on ($ ".pic-thumbnail") "click"
    (lambda (event)
      (let ((target (jcurrent-target event)))
        (zoom-pic target))))

(on (%host-ref "$(window)") "resize"
    (lambda (event)
      (let ((pic ($ "#zoomed-pic img")))
        (load-image (jattr pic "src")
                    (jattr pic "data-pic-id")))))

(on ($ "#close-zoomed-pic") "click" close-zoomed-pic)

(on ($ "#prev-pic") "click" prev-pic)

(on ($ "#next-pic") "click" next-pic)

(on ($ "#edit-pic-info") "click"
    (lambda ()
      (if pic-info-edit-mode?
          (render-pic-info)
          (set-pic-info-editable!))))

(live-on ($ "#content") "click" "#save-pic-info" save-pic-info)

(live-on ($ "#content") "click" "#cancel-edit-pic-info" render-pic-info)

(live-on ($ "#content") "click" ".remove-typeahead-icon"
         (lambda (event)
           (jremove (jparent (jcurrent-target event)))))

(live-on ($ "#content") "click" ".add-typeahead-icon"
         (lambda (event)
           (let* ((this (jcurrent-target event))
                  (ta-list (jsiblings this "ul"))
                  (class (jattr this "data-class")))
             (jappend ta-list
                      (sxml->html
                       (render-typeahead-input class ""))))))

(define (setup-typeahead-listener!)
  (%inline .autocomplete ($ ".album-typeahead")
           (% "serviceUrl" "/db/albums"))
  (%inline .autocomplete ($ ".tag-typeahead")
           (% "serviceUrl" "/db/tags")))



;;; Key bindings
(define (handle-keypress event)
  (if (jis ($ "#zoomed-pic") ":visible")
      (case (.keyCode event)
        ((37) ;; left arrow
         (unless pic-info-edit-mode?
           (prev-pic event)))
        ((39) ;; right arrow
         (unless pic-info-edit-mode?
           (next-pic event)))
        ((27) ;; esc
         (if pic-info-edit-mode?
             (render-pic-info)
             (close-zoomed-pic)))
        ((69) ;; e
         (unless pic-info-edit-mode?
           (set-pic-info-editable!)))
        ((83) ;; s
         (when (.ctrlKey event)
           (save-pic-info)
           (%inline .preventDefault event))))
      (case (.keyCode event)
        ((37) ;; left arrow
         (prev-thumbnail))
        ((39) ;; righ arrow
         (next-thumbnail))
        ((13) ;; enter
         (zoom-pic (current-thumbnail)))))
  #t)

(set! document.onkeydown (callback handle-keypress))

;; Focus the first thumbnail
(jfocus (jfirst ($ ".pic-thumbnail")))
