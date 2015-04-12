(define (edit-tag event)
  (let* ((this (jcurrent-target event))
         (tag (jtext (jfind (jparent this) "a"))))
    (jtext! ($ "#tag-old-text") tag)
    (jval! ($ "#tag-new-text") tag)
    (jattr! ($ "#tag-remove") "checked" #f)
    (%inline .modal ($ "#tag-edit-modal") "show")))

(define (save-tag event)
  (let* ((this (jcurrent-target event))
         (old-tag (jtext ($ "#tag-old-text")))
         (new-tag (jval ($ "#tag-new-text")))
         (tag-listed (jfilter (jchildren ($ "#tags-list"))
                              (lambda (idx elt)
                                (let ((link (jfind ($ elt) "a")))
                                  (and (equal? (jtext link) old-tag)
                                       link))))))
    (debug (conc "save-tag: old-tag: " old-tag ", new-tag: " new-tag))
    (cond
     ((or (jis ($ "#tag-remove") ":checked")
          (equal? new-tag ""))
      (remote-write (conc "/remove-tag/" old-tag) "")
      (jremove tag-listed))
     (else
      (remote-write (conc "/edit-tag/" old-tag "/" new-tag))
      (let ((edit-icon (jfind tag-listed "span"))
            (link (jfind tag-listed "a")))
        (jattr! link "href" (conc "/filter/by-tags/?include-tags=" new-tag))
        (jtext! link new-tag))))
    (%inline .modal ($ "#tag-edit-modal") "hide")))


;;;
;;; Event handlers
;;;

(on ($ ".edit-tag") "click" edit-tag)

(on ($ "#save-tag") "click" save-tag)
