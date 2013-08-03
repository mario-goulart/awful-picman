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
