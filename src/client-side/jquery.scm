(define (http-get url handler)
  ((native (%host-ref 'jQuery.get)) (jstring url) (callback handler)))

(define (http-post url data)
  ((native (%host-ref 'jQuery.post)) (jstring url) data))

(define ($ selector)
  (%inline jQuery (jstring selector)))

(define (jval jobj)
  (%inline .val jobj))

(define (jval! jobj val)
  (%inline .val jobj (jstring val)))

(define (jtext jobj)
  (%inline .text jobj))

(define (jtext! jobj text)
  (%inline .text jobj (jstring text)))

(define (jhtml jobj)
  (%inline .html jobj))

(define (jhtml! jobj html)
  (%inline .html jobj (jstring html)))

(define (jattr jobj attr)
  (%inline .attr jobj (jstring attr)))

(define (jattr! jobj attr val)
  (%inline .attr jobj (jstring attr) (jstring val)))

(define (on jobj event handler)
  (%inline .on jobj (jstring event) (callback handler)))

(define (live-on ancestor event selector handler)
  (%inline .on ancestor (jstring event) (jstring selector) (callback handler)))

(define (on-document-ready proc)
  (%inline "$(document).ready" proc))

(define (jcurrent-target event)
  ($ (.currentTarget event)))

(define (jshow jobj)
  (%inline .show jobj))

(define (jhide jobj)
  (%inline .hide jobj))

(define (jis jobj attr)
  (%inline .is jobj (jstring attr)))

(define (jfocus jobj)
  (%inline .focus jobj))

(define (jhas-class? jobj class)
  (%inline .hasClass jobj (jstring class)))

(define (jfirst jobj)
  (%inline .first jobj))

(define (jremove jobj)
  (%inline .remove jobj))

(define (jsiblings jobj . selector)
  (if (null? selector)
      (%inline .siblings jobj)
      (%inline .siblings jobj (car selector))))

(define (jappend jobj content)
  (%inline .append jobj (jstring content)))

(define (jparent jobj)
  (%inline .parent jobj))
