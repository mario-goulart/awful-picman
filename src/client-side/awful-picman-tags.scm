(define (edit-tag event)
  (let* ((this (jcurrent-target event))
         (tag (jattr this "data-tag")))
    (jattr! ($ "#save-tag") "data-old-tag" tag)
    (jval! ($ "#tag-new-text") tag)
    (jattr! ($ "#tag-remove") "checked" #f)
    (%inline .modal ($ "#tag-edit-modal") "show")))

(define (save-tag event)
  (let* ((this (jcurrent-target event))
         (old-tag (jattr this "data-old-tag"))
         (new-tag (jval ($ "#tag-new-text")))
         ;; FIXME: just a reminder that this data-tag thing is
         ;; completely broken (see tag.cm)
         (tag-listed (jfind ($ "#tags-list") (conc "[data-tag=\"" old-tag "\"]"))))
    (debug (conc "save-tag: old-tag: " old-tag ", new-tag: " new-tag))
    (cond
     ((or (jis ($ "#tag-remove") ":checked")
          (equal? new-tag ""))
      (remote-write (conc "/remove-tag/" old-tag) "")
      (jremove (jparent tag-listed)))
     (else
      (remote-write (conc "/edit-tag/" old-tag "/" new-tag))
      (let* ((parent (jparent tag-listed))
             (link (jfind parent "a"))
             (edit-icon (jfind parent "span")))
        (jattr! link "href" (conc "/filter/by-tags/?include-tags=" new-tag))
        (jtext! link new-tag)
        (jattr! edit-icon "data-tag" new-tag))))
    (%inline .modal ($ "#tag-edit-modal") "hide")))


;;;
;;; Event handlers
;;;

(on ($ ".edit-tag") "click" edit-tag)

(on ($ "#save-tag") "click" save-tag)
