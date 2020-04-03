#lang racket

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                    (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))



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



;;;;;;;;;;;;


(define pairs (list '(A 2)
                    '(BOOM 1)
                    '(GET 2)
                    '(JOB 2)
                    '(NA 16)
                    '(SHA 3)
                    '(YIP 9)
                    '(WAH 1)))

(define rock-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(display "Number of symbols in raw message: ")
(length rock-song) ;; 36

(display "Length of encoded message: ")
(length (encode rock-song (generate-huffman-tree pairs))) ;; 84 bits

;; Using fixed encoding:
;; We need to encode 8 symbols in total. This can be done using 2^3 = 8 codes in total, each has a length of 3.
;; For example A = 000, B = 001, etc.
;; Since our message contains 36 symbols, we need 36*3=108 bits vs 84 bits of variable length encoding.
