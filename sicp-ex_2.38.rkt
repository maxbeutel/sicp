#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
  

;;;;;;;;;;;;

(fold-right + 0 (list 1 2 3))

(fold-left + 0 (list 1 2 3))

(fold-right / 1 (list 3))
;; result is 3

(fold-right / 1 (list 1 2 3))
;; result is
;; 1 / (2 / (3 / 1)
;; 1 / (2 / 3)
;; 3/2

(fold-left / 1 (list 1 2 3))
;; result is
;; (1 / 2) / 3
;; (1/2) * 1/3
;; 1/6

(fold-right list nil (list 1 2 3))
;; result is
;; (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;; result is
;; (((() 1) 2) 3)

;; Property for fold-left == fold-right
;; operator must be commutative 

