#lang racket

(define (enumerate-interval lo hi)
  (if (> lo hi)
      '()
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (flat-map proc sequence)
  (foldr append '() (map proc sequence)))

(define (remove item L)
  (cond ((null? L) L)
        ((= item (car L)) (remove item (cdr L) ))
        (else (cons (car L)
                    (remove item (cdr L))))))

;;;;;;;;

 ; k-tuples of [1..n] 

  
 ; application to the case of 3-tuples


 (define (triples-of-sum s n)
 (define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))  
    (define (unique-tuples n k) 
     (cond ((< n k) '()) 
           ((= k 0) (list '())) 
           (else (append (unique-tuples (- n 1) k) 
                         (map (lambda (tuple) (cons n tuple)) 
                              (unique-tuples (- n 1) (- k 1))))))) 
     (filter (lambda (seq) (= (accumulate + 0 seq) s)) 
             (unique-tuples n 3))) 


;;;;;;;;

(define N 100)
(define S 10)

(define triples (flat-map (lambda (i)
       (flat-map (lambda (j)
              (map (lambda (k)
                     (list i j k))
                   (enumerate-interval 1 (- j 1)  )))  ;;;;; @TODO why i-1??
            (enumerate-interval 1 (- i 1)  )))
     (enumerate-interval 1 N)))

(define (i triple) (car triple))
(define (j triple) (cadr triple))
(define (k triple) (caddr triple))

(length (filter (lambda (triple)
                  (= S (+ (i triple) (j triple) (k triple))))
                triples))

(triples-of-sum S N) 

