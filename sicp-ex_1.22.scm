(define (next d)
  (if (= d  2) 3
      (+ d 2)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (if (> (square d) n)
      n
      (if (divides? d n)
          d
          (find-divisor n (next d)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

;; ==========

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " is prime *** took ")
  (display elapsed-time)
  (display " sec")
  true)

;; ==========

(define (show-done)
  (newline)
  (display "DONE"))

(define (search-for-primes start)
  (search-for-primes-iter start 0 3))

(define (search-for-primes-iter cur numFound max)
  (if (= numFound max)
      (show-done)
      (if (timed-prime-test cur)
          (search-for-primes-iter (+ cur 1) (+ numFound 1) max)
          (search-for-primes-iter (+ cur 1) numFound max))
      ))

;; ==========

;; (timed-prime-test 25)
;; (timed-prime-test 1999)
;; (timed-prime-test 68718952447)
;; (timed-prime-test 10888869450418352160768000001)

;; Too small numbers
;; (search-for-primes (* 1 1000))
;; (search-for-primes (* 10 1000))
;; (search-for-primes (* 100 1000))
;; (search-for-primes (* 1000 1000))

;; (newline)

(search-for-primes (* 1 1000 1000 1000))
(search-for-primes (* 10 1000 1000 1000))
(search-for-primes (* 100 1000 1000 1000))
(search-for-primes (* 1000 1000 1000 1000))
