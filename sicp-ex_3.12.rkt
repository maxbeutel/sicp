#lang sicp

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(append '(1 2) '(3 4))

(last-pair '(1))

(last-pair '(1 2))

(last-pair '(1 2 3))

(define (append! x y)
  (display "last-pair\n")
  (display (last-pair x))
  (display "\n------\n")
  (set-cdr! (last-pair x) y)
  x)

(define x '(1 2))
(define y '(3 4))

(append! x y)

x

y





;;;;;;;

;; (define x (list 'a 'b))

;; (define y (list 'c 'd))

;; (define z (append x y))

;; z
;; ;; (a b c d)

;; (cdr x)
;; ;; (b)

;; (define w (append! x y))

;; w
;; ;; (a b c d)

;; (cdr x)
;; ;; (b c d) -> because x was modified
