#lang racket

#|

Ben's implementation of a complex number, using magnitude/angle:

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

|#

(define (square n) (* n n))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unkown op --- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define z (make-from-mag-ang 4 90))

(apply-generic 'real-part z)

(apply-generic 'imag-part z)

(apply-generic 'magnitude z)

(apply-generic 'angle z)
