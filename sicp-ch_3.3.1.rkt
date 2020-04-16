#lang sicp

(define x '((a b) c d))

(define y '(e f))

(define z (cons y (cdr x)))

z

(set-car! x y)
(display "x is now\n")
x

(set-cdr! x y)
(display "x is now\n")
x

z
