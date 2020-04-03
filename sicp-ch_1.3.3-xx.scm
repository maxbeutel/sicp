(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f initial)
  (define (close-enough? a b tolerance)
    (< (abs (- a b)) tolerance))
  (define (try x)
    (display "\nTrying: ")
    (display x)
    (let ((next (f x))
          (tolerance 0.00001))
      (if (close-enough? x next tolerance)
          next
          (try next))))
  (try initial))

;;;;;;;;;;

(define A 2)

(fixed-point (lambda (y) (average y (/ A y)))
             1.0)
