#lang racket

(require rackunit)

;; Basic tree accessors

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; Utility

(define (tree->list T)
  (define (list-add W L)
    (if (null? W)
        L
        (list-add (left-branch W)
                  (cons (entry W)
                        (list-add (right-branch W) L)))))
  (list-add T '()))


(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))


(define (union-ordered-lists set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (union-ordered-lists (cdr set1) set2)))
                      ((< x2 x1) (cons x2 (union-ordered-lists set1 (cdr set2))))
                      (else (cons x1 (union-ordered-lists (cdr set1) (cdr set2)))))))))

;; Tree functions

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (list->tree
               (union-ordered-lists
                (tree->list set1)
                (tree->list set2))))))

;;;;;;;;;;;;;;


(define T1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define T2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define T3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(check-equal? (union-set '()
                         '()) '())

(check-equal? (union-set '() T1) T1)

(check-equal? (union-set T1 T1)
              '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))
