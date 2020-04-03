#lang racket

(define (prime? n)
  (if (= n 1)
      false
      (= (smallest-divisor n) n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (if (> (square d) n)
      n
      (if (divides? d n)
          d
          (find-divisor n (+ d 1)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      '()
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flat-map proc sequence)
  (accumulate append '() (map proc sequence)))

(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j)
                     (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
     (filter prime-sum? (unique-pairs n))))
             
(display "Prime-sum pairs:\n")
(prime-sum-pairs 6)




