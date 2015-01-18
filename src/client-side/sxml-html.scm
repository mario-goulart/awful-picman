(define (inc i)
  (+ i 1))

(define universal-conversion-rules*
  `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key value) (enattr attr-key value))))
      . ,(lambda (trigger value) (cons '@ value)))
    (*default* . ,(lambda (tag elems) (entag tag elems)))
    (*text* . ,(lambda (trigger str)
                 (if (string? str) (string->goodHTML str) str)))
    (n_         ; a non-breaking space
     . ,(lambda (tag elems)
          (cons "&nbsp;" elems)))))

(define (pre-post-order* tree bindings)
  (let* ((default-binding (assq '*default* bindings))
         (text-binding (or (assq '*text* bindings) default-binding))
         (text-handler                  ; Cache default and text bindings
           (and text-binding
             (if (procedure? (cdr text-binding))
                 (cdr text-binding) (cddr text-binding)))))
    (let loop ((tree tree))
      (cond
        ((null? tree) '())
        ((not (pair? tree))
          (let ((trigger '*text*))
            (if text-handler (text-handler trigger tree)
              (error "Unknown binding for " trigger " and no default"))))
        ((not (symbol? (car tree))) (map loop tree)) ; tree is a nodelist
        (else                           ; tree is an SXML node
          (let* ((trigger (car tree))
                 (binding (or (assq trigger bindings) default-binding)))
            (cond
              ((not binding)
                (error "Unknown binding for " trigger " and no default"))
              ((not (pair? (cdr binding)))  ; must be a procedure: handler
                ((cdr binding) trigger (map loop (cdr tree))))
              ((eq? '*preorder* (cadr binding))
                ((cddr binding) (car tree) (cdr tree)))
              ((eq? '*preorder/ss* (cadr binding))
                ((cddr binding) (car tree) (cdr tree) bindings))
              ((eq? '*macro* (cadr binding))
                (loop ((cddr binding) (car tree) (cdr tree))))
              (else                         ; (cadr binding) is a local binding
                ((cddr binding) trigger
                 (pre-post-order* (cdr tree) (append (cadr binding) bindings)))
                ))))))))

; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;       (make-char-quotator
;           '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
        (and (< i (string-length str))
             (if (memv (string-ref str i) charset) i
                 (loop (inc i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
        (if (not bad-pos) str   ; str had all good chars
            (let loop ((from 0) (to bad-pos))
              (cond
               ((>= from (string-length str)) '())
               ((not to)
                (cons (substring str from (string-length str)) '()))
               (else
                (let ((quoted-char
                       (cdr (assv (string-ref str to) char-encoding)))
                      (new-to
                       (index-cset str (inc to) bad-chars)))
                  (if (< from to)
                      (cons
                       (substring str from to)
                       (cons quoted-char (loop (inc to) new-to)))
                      (cons quoted-char (loop (inc to) new-to))))))))))
))

; procedure: entag TAG ELEMS
; Create the HTML markup for tags.
; This is used in the node handlers for the post-order function, see
; above.

(define (entag tag elems)
  (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
    (list #\newline #\< tag (cdar elems) #\>
      (and (pair? (cdr elems))
        (list (cdr elems) "</" tag #\>)))
    (list #\newline #\< tag #\> (and (pair? elems) (list elems "</" tag #\>))
      )))

; procedure: enattr ATTR-KEY VALUE
; Create the HTML markup for attributes.
; This and entag are being used in the node handlers for the post-order function, see
; above.

(define (enattr attr-key value)
  (if (null? value) (list #\space attr-key)
    (list #\space attr-key "=\"" value #\")))

; procedure: string->goodHTML STRING
; Given a string, check to make sure it does not contain characters
; such as '<' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.

(define string->goodHTML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

(define universal-conversion-rules
  `((@
     ((*default*       ; local override for attributes
       . ,(lambda (attr-key . value) (enattr attr-key value))))
     . ,(lambda (trigger . value) (cons '@ value)))
    (*default* . ,(lambda (tag . elems) (entag tag elems)))
    (*text* . ,(lambda (trigger str)
                 (if (string? str) (string->goodHTML str) str)))
    (n_         ; a non-breaking space
     . ,(lambda (tag . elems)
          (cons "&nbsp;" elems)))))

; procedure: SRV:send-reply FRAGMENT ...
;
; Output the 'fragments'
; The fragments are a list of strings, characters,
; numbers, thunks, #f, #t -- and other fragments.
; The function traverses the tree depth-first, writes out
; strings and characters, executes thunks, and ignores
; #f and '().
; The function returns #t if anything was written at all;
; otherwise the result is #f
; If #t occurs among the fragments, it is not written out
; but causes the result of SRV:send-reply to be #t

(define (SRV:send-reply port . fragments)
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments) port)
        (loop (cdr fragments) #t)))))


(define sxml->html*
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (let ((out (open-output-string)))
        (SRV:send-reply out (pre-post-order* sxml rules))
        (get-output-string out)))))

(define (sxml->html sxml)
  (jstring (sxml->html* sxml)))
