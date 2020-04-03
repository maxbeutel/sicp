(define (average a b)
  (/ (+ a b) 2))

(define (print guess)
  (display "# Current guess is ")
  (display guess)
  (newline))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (print guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;;;;;;;;;

(define (solve-no-damp y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
               10))

(define (solve-damp y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x))))
               10))

(solve-no-damp 1000)

(solve-damp 1000)
