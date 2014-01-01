(define (render-dynamic-input type idx widget-id val #!optional prepend-br?)
  `(,(if prepend-br?
         '(br)
         '())
    (input (@ (type "text")
              (class ,(sprintf "~a-widget-~a ~a" type widget-id type))
              (id ,(sprintf "~a-~a-~a" type idx widget-id))
              (data-provide "typeahead")
              (value ,val)))))

(define (render-dynamic-input+ type widget-id)
  `((span (@ (id ,(sprintf "~a-widget-placeholder-~a" type widget-id))))
    (a (@ (href "#")
          (class ,(sprintf "add-~a-widget" type))
          (id ,(sprintf "add-~a-~a" type widget-id)))
       (span (@ (class "badge badge-info"))
             "+"))))

(define (render-dynamic-inputs type widget-id inputs)
  (let* ((len-inputs (length inputs))
         (get-val (lambda (idx)
                    (if (< idx len-inputs)
                        (list-ref inputs idx)
                        ""))))
    `(,(if (zero? len-inputs)
           (render-dynamic-input type 0 widget-id "")
           (intersperse
            (map (lambda (i)
                   (render-dynamic-input type i widget-id (get-val i)))
                 (iota len-inputs))
            '(br)))
      ,(render-dynamic-input+ type widget-id))))

(define (add-dynamic-input-javascript-utils)
  (add-javascript "

get_max_dynamic_input_idx = function(type, widget_id) {
    return Math.max.apply(Math, $.map($('.' + type + '-widget-' + widget_id), function(i) {
        return i.id.split('-')[1];
    }));
}

get_dynamic_inputs = function(type, widget_id) {
    var elts = $.map($('.' + type + '-widget-' + widget_id), function(i) { return i; });
    return $.map(elts, function(i) { return $(i).val(); });
}
"))

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
          (with-request-variables (type widget-id next-idx)
            (render-dynamic-input type next-idx widget-id "" 'prepend-br)))
        prelude: (string-append
                  (sprintf "var widget_id = $(this).attr('id').replace(/^add-~a-/, '');" type)
                  (sprintf "var next = get_max_dynamic_input_idx('~a', widget_id) + 1;" type))
        arguments: `((widget-id . "widget_id")
                     (type      . ,(sprintf "'~a'" type))
                     (next-idx  . "next"))
        success: (string-append
                  (sprintf "$(response).insertBefore('#~a-widget-placeholder-' + widget_id);" type)
                  (sprintf "$('#~a-' + next + '-' + widget_id).typeahead({~a});"
                           type typeahead-source-js)
                  (sprintf "$('#~a-' + next + '-' + widget_id).focus();" type))))
