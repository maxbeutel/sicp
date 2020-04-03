(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

;; ==========

(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder (square (expmod base (/ exp 2) m)) m))
   (else
    (remainder (* base (expmod base (- exp 1) m)) m)))
   ))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;; ==========

;; Fermat-Pseudoprime
(fast-prime? 341 10)
;; Carmichael number
(fast-prime? 561 10)
(fast-prime? 17 10)
(fast-prime? 25 10)
(fast-prime? 18 10)
(fast-prime? 19 10)
