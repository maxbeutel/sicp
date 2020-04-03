(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

;;;;;;;;;;

(define (compose f g)
  (lambda (x)
    (f (g x))))


;; Return a function that when called,
;; calls f n-times nested
(define (repeated f n)
  (define (loop cur_f i)
    (if (= i n)
        cur_f
        (loop (compose f cur_f) (+ i 1))))
  (loop f 1))

;;;;;;;;;;

((repeated square 3) 5)
