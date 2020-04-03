#lang racket

(require rackunit)

;; Leaves
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; Tree

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (choose? symbol object)
    (if (leaf? object)
        (eq? symbol (symbol-leaf object))
        (contains (symbols object) symbol)))
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((choose? symbol left) (cons 0 (encode-symbol symbol left)))
              ((choose? symbol right) (cons 1 (encode-symbol symbol right)))
              (else (error "bad symbol -- encode-symbol" symbol))))))

(define (contains list item)
  (cond ((null? list) false)
        ((eq? (car list) item) true)
        (else (contains (cdr list) item))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(check-equal? (encode '(A B A B B C A) sample-tree) '(0 1 0 0 1 0 1 0 1 1 1 0))

(check-equal? (contains '(A B C) 'A) true)
(check-equal? (contains '(A B C) 'Y) false)

(check-equal? (encode-symbol 'A sample-tree) '(0))
(check-equal? (encode-symbol 'B sample-tree) '(1 0))
(check-equal? (encode-symbol 'C sample-tree) '(1 1 1))
(check-equal? (encode-symbol 'D sample-tree) '(1 1 0))

;(encode-symbol 'X sample-tree)
