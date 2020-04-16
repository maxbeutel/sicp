#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;;;;;;;

(define x (list 'a 'b 'c))

(define z (make-cycle x))

(cdr z)

(cddr z)

(cdddr z)


#|

Diagram for x:

x -> |a|*| -> |b|*| -> |c|/|

Diagram for z:

z -> x -> |a|*| -> |b|*| -> |c|*| -> x

|#
