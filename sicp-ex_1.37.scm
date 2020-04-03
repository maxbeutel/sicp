(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;;;;;;;;;

(define (cont-frac-rec n d k)
  (define (f i)
    (let ((n-value (n i))
          (d-value (d i))
          (next (+ i 1)))
      (if (= i k)
          0
          (/ n-value
             (+ d-value (f next))))))
  (f 1))

(define (cont-frac-iter n d k)
  (define (loop i prev)
    (let ((n-value (n i))
          (d-value (d i)))
      (if (= i 0)
        prev
        (loop (- i 1)
              (/ n-value (+ d-value prev))))))
  (loop k 0))

;;;;;;;;;;

(define k 15)

;; 1/phi = 0.6180339887498948

(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               k)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)
