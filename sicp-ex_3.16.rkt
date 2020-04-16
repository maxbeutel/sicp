#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;;;;;;;;;

#|

Exactly 3:

u -> |a|*| -> |b|*| -> |c|/|

|#

(define u '(a b c))

(count-pairs u)

#|

Exactly 4:

v1 -> |a|*|
         |
         v2 -> |b|/|

v  -> |v1|v2|

|#


(define v1 '(a))
(define v2 '(b))
(define v (cons v1 v2))
(set-cdr! v1 v2)

(count-pairs v)

#|

Exactly 7:

w1 -> |a|/|

w2 -> |*|*|
       | |
      w1 w1

w -> |*|*|
      | |
     w2 w2

|#

(define w1 '(a))
(define w2  (cons w1 w1))
(define w (cons w2 w2))

(count-pairs w)

#|

Never returns:

x1 -> |c|x3|

x2 -> |b|x1|

x -> |a|x2|

|#

(define x1 '(b))
(define x2 '(a))
(define x (cons x1 x2))
(set-cdr! x2 x)
x

;(count-pairs x)
