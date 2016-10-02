(read-conf-from-server!)

;; Keep track of rotated images to avoid caching issues.  Items of
;; this list are strings "pic-<id>".
(define *rotated-pics* '())

(define (was-rotated? pic-id)
  (member pic-id *rotated-pics*))

(define (add-to-rotated-pics! pic-id)
  (unless (member pic-id *rotated-pics*)
    (set! *rotated-pics* (cons pic-id *rotated-pics*))))

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
              (car default))))))

(define pic-info-edit-mode? #f)

(define (enter-pic-info-edit-mode!)
  (set! pic-info-edit-mode? #t)
  (shade-icon ($ "#edit-pic-info")))

(define (leave-pic-info-edit-mode!)
  (set! pic-info-edit-mode? #f)
  (unshade-icon ($ "#edit-pic-info")))

(define (render-pic-path pic-data)
  (let* ((dir (pic-data 'dir))
         (filename (pic-data 'filename))
         (path (if (equal? dir ".")
                   filename
                   (string-append dir "/" filename))))
    `(p (code ,(if (equal? dir ".")
                   filename
                   ;; FIXME: link all the dir chain
                   `((a (@ (href ,(string-append "/folders" "/" dir)))
                        ,(string-append dir "/"))
                     ,filename))))))

(define (render-pic-info pic-data-alist . for-batch-edit?)
  (let* ((pic-data (make-pic-data-getter pic-data-alist))
         (id-prefix (if (null? for-batch-edit?)
                        "pic-"
                        "pic-template-"))
         (prefix (lambda (id-suffix)
                   (string-append id-prefix id-suffix)))
         (use-ocr? (and ocr-installed?
                        (null? for-batch-edit?)
                        (ocr-supported-pic-format? (pic-data 'filename)))))
    (sxml->html
     `(div (@ (id ,(prefix "form")))
           (h4 ,(_ "Description")
               ,(if use-ocr?
                    `(" " (span (@ (id ,(prefix "maybe-ocr-icon")))))
                    '()))
           ,(if use-ocr?
                `(span (@ (id "ocr-controls"))
                       "") ;; this space is required.  Probably a bug somewhere.
                '())
           (p (@ (id ,(prefix "description-wrapper")))
              ,(pic-data 'description ""))
           (h4 ,(_ "Date"))
           (p (@ (id ,(prefix "date")))
              ,(let ((date (pic-data 'date)))
                 (if date
                     (apply render-date date)
                     "")))
           (h4 ,(_ "Tags"))
           (div (@ (id ,(prefix "tags")))
                ,(itemize (pic-data 'tags '())))
           (h4 ,(_ "Albuns"))
           (div (@ (id ,(prefix "albums")))
                ,(itemize (pic-data 'albums '())))
           ,(if (null? for-batch-edit?)
                (render-pic-path pic-data)
                '())
           (div (@ (id "pic-info-edit-button-bar")
                   (style "visibility: hidden;"))
                (button (@ (id "cancel-edit-pic-info"))
                        ,(_ "Cancel"))
                (button (@ (id "save-pic-info"))
                        ,(_ "Save")))))))

(define (read&render-pic-info pic-id)
  (remote-read (string-append "/read-pic-info/" pic-id)
               (lambda (pic-data)
                 (leave-pic-info-edit-mode!)
                 (jhtml! ($ "#pic-info") (render-pic-info pic-data)))))

(define (set-pic-info-editable! pic-data-alist . for-batch-edit?)
  (let* ((pic-data (make-pic-data-getter pic-data-alist))
         (id-prefix (if (null? for-batch-edit?)
                        "pic-"
                        "pic-template-"))
         (prefix (lambda (id-suffix)
                   (string-append id-prefix id-suffix)))
         (hash-prefix (lambda (id-suffix)
                        (string-append "#" id-prefix id-suffix)))
         (use-ocr? (and ocr-installed?
                        (null? for-batch-edit?)
                        (ocr-supported-pic-format? (pic-data 'filename)))))
    (let* ((tags (pic-data 'tags '()))
           (albums (pic-data 'albums '())))
      (when use-ocr?
        (jhtml! ($ (hash-prefix "maybe-ocr-icon"))
                (sxml->html
                 `(span (@ (id "show-ocr-controls")
                           (title "OCR")
                           (class "show-ocr-controls glyphicon glyphicon-eye-open"))))))
      (jhtml! ($ (hash-prefix "description-wrapper"))
              (sxml->html
               `(textarea (@ (id ,(prefix "description"))
                             (rows 1))
                          ,(pic-data 'description ""))))
      (jhtml! ($ (hash-prefix "date"))
              (let* ((date (pic-data 'date))
                     (decade (if date (or (car date) "") ""))
                     (year (if date (or (cadr date) "") ""))
                     (month (if date (or (caddr date) "") ""))
                     (day (if date (or (cadddr date) "") "")))
                (sxml->html
                 `(,(combo-box (prefix "date-decade") (iota 10 1920 10) #t decade "decade")
                   ,(combo-box (prefix "date-year") (iota 10) #t year "year")
                   ,(combo-box (prefix "date-month") (iota 12 1) #t month "month")
                   ,(combo-box (prefix "date-day") (iota 31 1) #t day "day")))))
      (jhtml! ($ (hash-prefix "tags"))
              (sxml->html (render-typeahead-inputs "tag-typeahead" tags)))
      (jhtml! ($ (hash-prefix "albums"))
              (sxml->html (render-typeahead-inputs "album-typeahead" albums)))
      (jfocus ($ (hash-prefix "description")))
      (setup-typeahead-listener!)))
  (when (null? for-batch-edit?)
    (%inline .css ($ "#pic-info-edit-button-bar") "visibility" "visible")))

(define (show-ocr-controls event)
  (let* ((pic-id (jattr ($ "#zoomed-pic img") "data-pic-id")))
    (debug (conc "show-ocr-controls, pic-id: " pic-id))
    (jhtml! ($ "#ocr-controls")
            (sxml->html
             `(div (@ (class "ocr-controls-bar"))
                   (,(_ "Language") ": "
                    ,(combo-box "ocr-lang" ocr-languages #f "eng" "ocr-lang")
                    (button (@ (id "run-ocr")) ,(_ "Run OCR"))))))))

(define (read&set-pic-info-editable!)
  (enter-pic-info-edit-mode!)
  (remote-read (string-append "/read-pic-info/" (get-zoomed-pic-id))
               (lambda (pic-data)
                 (set-pic-info-editable! pic-data))))

(define (drop-pic-id-prefix pic-id)
  ;; Given "pic-<id>", return "<id>"
  (substring pic-id 4))

(define (get-zoomed-pic-id)
  (drop-pic-id-prefix (jattr ($ "#zoomed-pic img") "data-pic-id")))

(define (get-selected-pic-ids)
  (map (lambda (elt)
         (string->number (%inline ".getAttribute" elt (jstring "data-pic-id"))))
       (vector->list (%inline .toArray ($ ".pic-select:checked")))))

(define (show-zoomed-pic)
  (let ((zoomed-pic-area-wrapper ($ "#zoomed-pic-area-wrapper"))
        (zoomed-pic-id (jattr ($ "#zoomed-pic img") "data-pic-id")))
    (%inline .css zoomed-pic-area-wrapper
             "top" (%host-ref "$(document).scrollTop()") "px;")
    (read&render-pic-info (get-zoomed-pic-id))
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
      (%inline .css ($ "#content") (% "margin-top" "0px"))
      (%inline .css ($ "#thumbnails") (% "opacity" 0))
      (jhide ($ "#main-navbar"))
      (jhide ($ ".breadcrumb"))
      (unshade-icon ($ "#edit-pic-info"))
      (%inline .addClass ($ "body") "modal-open")
      (jshow ($ "#pic-info-wrapper")))))

(define-native loadImage)

(define (load-image src id)
  (let ((src (if (was-rotated? id)
                 (jstring (conc src "?" (milliseconds))) ;; Avoid cache
                 src)))
    (debug (conc "Loading image. src=" src ", id=" id))
    (loadImage src
               (callback
                (lambda (img)
                  (jattr! ($ img) "data-pic-id" id)
                  (jhtml! ($ "#zoomed-pic") (scale-pic img))
                  (show-zoomed-pic))))))

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

(define (rotate-pic! event)
  (let* ((pic-id (jattr ($ "#zoomed-pic img") "data-pic-id"))
         (hash-pic-id (string-append "#" pic-id))
         (id (drop-pic-id-prefix pic-id)))
    (remote-read (string-append "/rotate-pic/" id)
                 (lambda (pic-path/dimensions)
                   (debug pic-path/dimensions)
                   (let ((pic-path (car pic-path/dimensions))
                         (thumb-dimension (cadr pic-path/dimensions))
                         (zoom-dimension (caddr pic-path/dimensions)))
                     (add-to-rotated-pics! pic-id)
                     (jattr! ($ hash-pic-id)
                             "src"
                             (conc "/thumbnails/" thumb-dimension "/" pic-path "?x=" (milliseconds)))
                     (load-image (conc "/thumbnails/" zoom-dimension "/" pic-path)
                                 pic-id))))))


(define (close-zoomed-pic)
  (leave-pic-info-edit-mode!)
  (let ((pic-id (jattr ($ "#zoomed-pic img") "data-pic-id")))
    (jhide ($ "#zoomed-pic-area-wrapper"))
    (jshow ($ "#main-navbar"))
    (jshow ($ ".breadcrumb"))
    (%inline .removeClass ($ "body") "modal-open")
    (%inline .css ($ "#content") (% "margin-top" "95px")) ;; FIXME: hardcoded
    (%inline .css ($ "#thumbnails") (% "opacity" 1))
    (debug (string-append "focusing " pic-id))
    (jfocus ($ (string-append "#" pic-id)))
    #f))

(define (save-pic-info for-batch-edit?)
  (let* ((id-prefix (if for-batch-edit?
                        "#pic-template-"
                        "#pic-"))
         (prefix (lambda (id-suffix)
                   (string-append id-prefix id-suffix)))
         (pic-id (if for-batch-edit?
                     (get-selected-pic-ids)
                     (string->number (get-zoomed-pic-id))))
         (description (jval ($ (prefix "description"))))
         (decade (jval ($ (prefix "date-decade"))))
         (year (jval ($ (prefix "date-year"))))
         (month (jval ($ (prefix "date-month"))))
         (day (jval ($ (prefix "date-day"))))
         (remove-null-string
          (lambda (class)
            (filter-map (lambda (elt)
                          (let ((val (.value elt)))
                            (and (not (equal? val ""))
                                 val)))
                        (vector->list (%inline .toArray ($ class))))))
         (tags (remove-null-string ".tag-typeahead"))
         (albums (remove-null-string ".album-typeahead")))
    (remote-write (if for-batch-edit?
                      "/write-pic-template"
                      (conc "/write-pic-info/" pic-id))
                  `((,(if for-batch-edit? 'pic-ids 'id) . ,pic-id)
                    (description . ,description)
                    (date ,@(map string->number (list decade year month day)))
                    (tags . ,tags)
                    (albums . ,albums)))
    (unless for-batch-edit?
      (read&render-pic-info pic-id)
      (unshade-icon ($ "#edit-pic-info"))
      (debug "saving pic info"))))

(define (toggle-pic-info-visibility)
  (let ((pic-info ($ "#pic-info-wrapper")))
    (if (jis pic-info ":visible")
        (jhide pic-info)
        (jshow pic-info))))

(define (run-ocr event)
  (let* ((pic-id (jattr ($ "#zoomed-pic img") "data-pic-id"))
         (hash-pic-id (string-append "#" pic-id))
         (id (drop-pic-id-prefix pic-id))
         (lang (jval ($ "#ocr-lang"))))
    (remote-read (string-append "/run-ocr/" lang "/" id)
                 (lambda (result)
                   (debug result)
                   (let ((descr ($ "#pic-description")))
                     (jval! descr
                            (string-append (jtext descr)
                                           "\n"
                                           (alist-ref 'msg result))))))))

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
        (if (string? (jattr target "data-zoomed"))
            ;; Only pic thumbnails have the data-zoomed attrib.
            ;; Directory and other-file icons don't.
            (zoom-pic target)
            ;; default handler for click, which should be invoked for
            ;; dir and other-file icons.
            #t))))

(on (%host-ref "$(window)") "resize"
    (lambda (event)
      (let ((pic ($ "#zoomed-pic img")))
        (load-image (jattr pic "src")
                    (jattr pic "data-pic-id")))))

(on ($ "#close-zoomed-pic") "click" close-zoomed-pic)

(on ($ "#prev-pic") "click" prev-pic)

(on ($ "#next-pic") "click" next-pic)

(on ($ "#rotate-pic") "click" rotate-pic!)

(on ($ "#edit-pic-info") "click"
    (lambda ()
      (if pic-info-edit-mode?
          (read&render-pic-info (get-zoomed-pic-id))
          (read&set-pic-info-editable!))))

(on ($ "#select-all") "click"
    (lambda ()
      (jprop! ($ ".pic-select") "checked" #t)))

(on ($ "#deselect-all") "click"
    (lambda ()
      (jprop! ($ ".pic-select") "checked" #f)))

(on ($ "#toggle-selection") "click"
    (lambda ()
      ;; FIXME: that feels a bit slow.  Can it be faster?
      (jtrigger ($ ".pic-select") "click")))

(on ($ "#select-from-to") "click"
    (lambda ()
      (let ((selected (vector->list
                       (%inline .toArray ($ ".pic-select:checked")))))
        (if (= 2 (length selected))
            (let* ((pics ($ ".pic-thumbnail"))
                   (pic-id1 (jattr ($ (car selected)) "data-pic-id"))
                   (pic-id2 (jattr ($ (cadr selected)) "data-pic-id"))
                   (pic1 ($ (string-append "#pic-" pic-id1)))
                   (pic2 ($ (string-append "#pic-" pic-id2)))
                   (from (%inline .index pics pic1))
                   (to (%inline .index pics pic2)))
              (let loop ((current (+ from 1)))
                (unless (= current to)
                  (let* ((pic ($ (vector-ref pics current)))
                         (checkbox (jfind (jparent pic) "input")))
                    (jprop! checkbox "checked" #t)
                    (loop (+ current 1))))))
            (%inline alert (_ "You must select exactly two pictures."))))))

(on ($ "#batch-edit") "click"
    (lambda ()
      (jhtml! ($ "#pic-template-form-container")
              (render-pic-info '() 'batch-edit))
      (set-pic-info-editable! '() 'batch-edit)
      (show-modal ($ "#pic-template-modal"))))

(on ($ "#save-pic-template") "click"
    (lambda ()
      (save-pic-info 'batch-edit)
      (jprop! ($ ".pic-select") "checked" #f)
      (hide-modal ($ "#pic-template-modal"))))

(live-on ($ "body") "click" "#save-pic-info"
         (lambda ()
           (save-pic-info #f)))

(live-on ($ "body") "click" "#run-ocr" run-ocr)

(live-on ($ "body") "click" "#show-ocr-controls" show-ocr-controls)

(live-on ($ "body") "click" "#cancel-edit-pic-info"
         (lambda ()
           (read&render-pic-info (get-zoomed-pic-id))))

(live-on ($ "body") "click" ".remove-typeahead-icon"
         (lambda (event)
           (jremove (jparent (jcurrent-target event)))))

(live-on ($ "body") "click" ".add-typeahead-icon"
         (lambda (event)
           (let* ((this (jcurrent-target event))
                  (ta-list (jsiblings this "ul"))
                  (class (jattr this "data-class")))
             (jappend ta-list
                      (sxml->html
                       `(li ,(render-typeahead-input class ""))))
             (jfocus (jfind (jlast (jsiblings this "ul")) "input")))
           (setup-typeahead-listener!)))

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
             (read&render-pic-info (get-zoomed-pic-id))
             (close-zoomed-pic)))
        ((69) ;; e
         (unless pic-info-edit-mode?
           (read&set-pic-info-editable!)))
        ((82) ;; r
         (unless pic-info-edit-mode?
           (rotate-pic!)))
        ((83) ;; C-s
         (when (.ctrlKey event)
           (save-pic-info #f)
           (%inline .preventDefault event)))
        ((88) ;; C-x
         (when (.ctrlKey event)
           (toggle-pic-info-visibility))))
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


;;;
;;; For the filtering stuff
;;;

(define (render-filter-input)
  (let* ((filter-tags ($ "#filter-tags"))
         (data-include-tags (jattr filter-tags "data-include-tags"))
         (data-exclude-tags (jattr filter-tags "data-exclude-tags")))
    ;; data-include-tags and data-exclude-tags are undefined when not
    ;; in tags page
    (when (and (not (void? data-include-tags))
               (not (void? data-exclude-tags)))
      (let ((include-tags (string-split data-include-tags #\tab))
            (exclude-tags (string-split data-exclude-tags #\tab)))
        (jhtml! ($ "#filter-input-container")
                (sxml->html
                 `(div (@ (class "filter-input"))
                       ,(_ "Show pictures tagged with")
                       ,(render-typeahead-inputs
                         "tag-typeahead include-tag-typeahead"
                         include-tags)
                       ,(_ "except those tagged with")
                       ,(render-typeahead-inputs
                         "tag-typeahead exclude-tag-typeahead"
                         exclude-tags)
                       (input (@ (id "filter-by-tags")
                                 (type "submit")
                                 (value ,(_ "Filter")))))))
        (%inline .autocomplete ($ ".include-tag-typeahead")
                 (% "serviceUrl" "/db/tags"))
        (%inline .autocomplete ($ ".exclude-tag-typeahead")
                 (% "serviceUrl" "/db/tags"))))))

(render-filter-input)

(on ($ "#filter-by-tags") "click"
    (lambda ()
      (let* ((url (string-append document.location.protocol
                                 "//"
                                 document.location.host
                                 "/filter/by-tags?")) ;; FIXME: hardcoded
             (encode-vals
              (lambda (class)
                (%inline "encodeURIComponent"
                         (jstring
                          (string-intersperse
                           (map (lambda (elt)
                                  (.value elt))
                                (vector->list (%inline .toArray ($ class))))
                           "\t")))))
             (query (string-append
                     "include-tags=" (encode-vals ".include-tag-typeahead")
                     "&"
                     "exclude-tags=" (encode-vals ".exclude-tag-typeahead"))))
        (%inline "window.location.assign" (jstring (string-append url query))))))
