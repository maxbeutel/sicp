(define (cube n)
  (* n n n))

(define (inc n)
  (+ n 1))

(define (identity x) x)

(define (tautology n) true)

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

;; ------------------

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum term a next b)
  (filtered-accumulate + tautology 0 term a next b))

(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

;; (define (sum-integers a b)
;;   (sum identity a inc b))

;; (define (sum-cubes a b)
;;   (sum cube a inc b))

;; (define (pi-sum a b)
;;   (define (pi-term x)
;;     (/ 1.0 (* x (+ x 2))))
;;   (define (pi-next x)
;;     (+ x 4))
;;   (sum pi-term a pi-next b))

;; ------------------

(sum-prime-squares 1 10)

;; (sum-integers 1 10)

;; (sum-cubes 1 10)

;; (pi-sum 1 1000)
