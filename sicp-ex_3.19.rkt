#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; We are traversing the list using cdr, so according to the specification,
;; we don't need to check for cycles in car.
;; Good explanation of the two pointer algorithm:
;; https://web.archive.org/web/20160323172739/http://learningarsenal.info/index.php/2015/08/24/find-loop-in-singly-linked-list/
;;
;;; We need to hande special cases:
;;
;; - The list itself is empty
;; - There is only one item in the list, that is (cdr slow) is '()
;; - When one of the pointers reach NULL, then we know that the end of the list can be reached.
;; - When the slow pointer catches up with the fast pointer, we know that the algorithm found a cycle.

(define (contains-cycle? L)
  (define (helper slow fast)
    (cond ((eq? slow fast) true)
          ((null? (cdr slow)) false)
          ((or (null? (cdr fast)) (null? (cddr fast))) false)
          (else (helper (cdr slow) (cddr fast)))))
  (if (null? L)
      false
      (helper L (cdr L))))

;;;;;;;;

(contains-cycle? '())

(contains-cycle? '(1))

(define x (list 'a 'b 'c))
(contains-cycle? x)

(define y (make-cycle x))
(contains-cycle? y)

(define u (list 'a))
(contains-cycle? u)

(define v (make-cycle u))
(contains-cycle? v)

(define r (list 'a 'b))
(contains-cycle? r)

(define s (make-cycle r))
(contains-cycle? r)
