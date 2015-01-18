(define debug-enabled? #t)

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
