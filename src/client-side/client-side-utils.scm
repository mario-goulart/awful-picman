(define debug-enabled? #t)

(define album-export-dir-suggestion #f)

(define (debug msg)
  (when debug-enabled?
    (%inline console.log
             (if (string? msg)
                 (jstring (string-append "[DEBUG] " msg))
                 msg))))

(define (remote-read url handler)
  (http-get url (lambda (data)
                  (let ((data (with-input-from-string data read)))
                    (handler data)))))

(define (remote-write url data)
  (http-post url
             (jstring
              (with-output-to-string
                (lambda ()
                  (write data))))))

(define (shade-icon jobj)
  (%inline .css jobj (% "opacity" "0.5")))

(define (unshade-icon jobj)
  (%inline .css jobj (% "opacity" "1")))

(define (->string obj)
  (cond ((number? obj)
         (number->string obj))
        ((boolean? obj)
         (if obj "#t" "#f"))
        ((symbol? obj)
         (symbol->string obj))
        (else obj)))

(define (conc . things)
  (apply string-append (map ->string things)))

(define (alist-ref field alist)
  (let ((pair (assoc field alist)))
    (and pair (cdr pair))))

(define (iota count . maybe-start+step)
  (let ((start (if (null? maybe-start+step)
                   0
                   (car maybe-start+step)))
        (step (if (or (null? maybe-start+step)
                      (null? (cdr maybe-start+step)))
                  1
                  (cadr maybe-start+step))))
    (let ((last-val (+ start (* (- count 1) step))))
      (do ((count count (- count 1))
           (val last-val (- val step))
           (ans '() (cons val ans)))
          ((<= count 0)  ans)))))

(define (filter-map f lis1)
  (let recur ((lis lis1))
    (if (null? lis) lis
        (let ((tail (recur (cdr lis))))
          (cond ((f (car lis)) => (lambda (x) (cons x tail)))
                (else tail))))))


(define (filter pred lis)
  (let recur ((lis lis))
    (if (null? lis)
        lis
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail)))
                (if (eq? tail new-tail)
                    lis
                    (cons head new-tail)))
              (recur tail))))))

(define (delete x l)
  (filter (lambda (i) (not (equal? i x))) l))

(define (%string-split chars sep)
  (let loop ((chars chars)
             (token '()))
    (if (null? chars)
        (if (null? token)
            '()
            (list (list->string (reverse token))))
        (let ((char (car chars)))
          (if (char=? char sep)
              (cons (list->string (reverse token))
                    (%string-split (cdr chars) sep))
              (loop (cdr chars) (cons char token)))))))

(define (string-split str sep)
  (delete "" (%string-split (string->list str) sep)))

(define (string-intersperse l sep)
  (cond ((null? l)
         "")
        ((null? (cdr l))
         (car l))
        (else
         (string-append (car l) sep (string-intersperse (cdr l) sep)))))

(define (sprintf fmt . args)
  ;; Cheap implementation of sprintf (only supports ~a, ~A and ~~)
  (let loop ((chars (string->list fmt))
             (args args)
             (str ""))
    (if (null? chars)
        str
        (let ((char (car chars)))
          (case char
            ((#\~)
             (if (null? (cdr chars))
                 (error 'sprintf
                        "Invalid format specification (missing specifier for ~)")
                 (let ((spec (cadr chars)))
                   (case  spec
                     ((#\~)
                      (loop (cddr chars)
                            args
                            (string-append str "~")))
                     (else
                      (if (null? args)
                          (error 'sprintf "too few arguments to format.")
                          (case spec
                            ((#\a #\A)
                             (loop (cddr chars)
                                   (cdr args)
                                   (string-append str (car args))))
                            (else
                             (error 'sprintf "Invalid specifier for ~")))))))))
            (else
             (loop (cdr chars)
                   args
                   (string-append str (string char)))))))))

(define (itemize items)
  (if (null? items)
      '()
      `(ul ,@(map (lambda (item)
                    `(li ,item))
                  items))))

(define (combo-box id options first-empty? default class)
  `(select (@ (id ,id)
              (name ,id)
              ,(if class
                   `(class ,class)
                   '()))
           ,@(map (lambda (opt)
                    (let ((val (if (pair? opt)
                                   (car opt)
                                   opt))
                          (text (cond ((list? opt)
                                       (cadr opt))
                                      ((pair? opt)
                                       (cdr opt))
                                      (else opt))))
                      `(option (@ (value ,val)
                                  ,(if (and default (equal? val default))
                                       '(selected)
                                       '()))
                               ,text)))
                  (if first-empty?
                      (cons "" options)
                      options))))

;; Modals
(define (show-modal jobj)
  (%inline .modal jobj "show"))

(define (hide-modal jobj)
  (%inline .modal jobj "hide"))


;; Typeahead
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


;;; awful-picman -specific

(define (read-conf-from-server!)
  (remote-read "/conf"
               (lambda (data)
                 (debug "Configuration data from server:")
                 (debug data)
                 (set! i18n-language
                   (case (string->symbol (alist-ref 'i18n-language data))
                     ((en en_US) i18n/en)
                     ((pt_BR) i18n/pt-br)
                     (else #f)))
                 (let ((dir-suggestion (alist-ref 'album-export-dir-suggestion data)))
                   (when dir-suggestion
                     (set! album-export-dir-suggestion dir-suggestion))))))
