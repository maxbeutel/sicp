(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube n)
  (* n n n))

;; ----------

(define (sum-cubes a b)
  (sum cube a inc b))

;; ----------

(define (integral f a b dx)
  (define (nex a_tmp) (+ a_tmp dx))
  (* (sum f (+ a (/ dx 2.0)) nex b) dx))

;; ----------

(define (simpson-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (* (coefficient k) (f (+ a (* k (h))))))
  (define (coefficient k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (* (sum y 0 inc n) (/ (h) 3.0)))

;; ----------

;(sum-cubes 1 10)

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simpson-integral cube 0 1 10)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
