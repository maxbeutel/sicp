#lang racket

(define (square n) (* n n))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op:
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define z (make-from-real-imag 4 5))

(apply-generic 'real-part z)

(apply-generic 'imag-part z)

(apply-generic 'angle z)
