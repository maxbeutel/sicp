(define (cube n)
  (* n n n))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(sum-integers 1 10)

(sum-cubes 1 10)

(pi-sum 1 1000)

;; --------

(define (inc n)
  (+ n 1))

(define (identity x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-integers2 a b)
  (sum identity a inc b))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(sum-integers2 1 10)

(sum-cubes2 1 10)

(pi-sum2 1 1000)
