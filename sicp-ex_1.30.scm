(define (cube n)
  (* n n n))

(define (inc n)
  (+ n 1))

(define (identity x) x)

;; ------------------

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; ------------------

(sum-integers 1 10)

(sum-cubes 1 10)

(pi-sum 1 1000)
