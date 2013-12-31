(define (render-tag tag tag-id)
  `(div (@ (class "tag-container")
           (id ,(conc "tag-container-" tag-id)))
        (span (@ (class "tag")
                 (id ,(conc "tag-" tag-id)))
              ,tag)
        (literal "&nbsp;")
        (a (@ (href ,(conc "#tag-modal-" tag-id))
              (data-toggle "modal"))
           (i (@ (class "icon-edit tag-edit"))))))

(define (render-tag-modal tag tag-id)
  `(div (@ (id ,(conc "tag-modal-" tag-id))
           (class "modal hide")
           (role "dialog")
           (tabindex "-1")
           (aria-labelledby ,tag))
        (div (@ (class "modal-header"))
             (button (@ (type "button")
                        (class "close")
                        (data-dismiss "modal")
                        (aria-hidden "true"))
                     ×)
             (h3 ,(_ "Edit tag")))
        (div (@ (class "modal-body"))
             (input (@ (type "text")
                       (id ,(conc "new-tag-" tag-id))
                       (value ,tag)))
             (hr)
             (p ,(_ "Remove tag?")
                (literal "&nbsp;")
                (input (@ (type "checkbox")
                          (id ,(conc "tag-remove-" tag-id)))))
             (br)
             (button (@ (id ,(conc "update-tag-" tag-id))
                        (class "btn update-tag"))
                     ,(_ "Submit"))
             (button (@ (data-dismiss "modal")
                        (class "btn tag-cancel"))
                     ,(_ "Cancel")))))

(define (render-tags)

  (ajax "/update-tag" ".update-tag" 'click
        update-tag!
        prelude: "var tag_id = $(this).attr('id').replace(/^update-tag-/, '');"
        arguments: `((tag-id   . "tag_id")
                     (remove?  . "$('#tag-remove-' + tag_id).is(':checked')")
                     (original-tag . "$('#tag-' + tag_id).text()")
                     (new-tag  . "$('#new-tag-' + tag_id).val()"))
        success: (string-append
                  "if ($('#tag-remove-' + tag_id).is(':checked'))"
                  "    $('#tag-container-' + tag_id).remove();"
                  "else"
                  "    $('#tag-' + tag_id).html($('#new-tag-' + tag_id).val());"
                  "$('#tag-modal-' + tag_id).modal('hide');"))

  `(,(render-top-bar 'tag)
    ,(let ((tags (db-tags)))
       (if (null? tags)
           `(div (@ (id "nothing-here"))
                 ,(_ "No tag has been created yet."))
           (map (lambda (tag id)
                  (list (render-tag-modal tag id)
                        (render-tag tag id)))
                tags
                (iota (length tags)))))))
