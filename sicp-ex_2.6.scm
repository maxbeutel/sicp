(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;;;;;;;;;;;;

(add-1 zero)

;;; See:
;;; https://jlongster.com/SICP-2.6--Church-Numerals
;;; https://www.cs.rice.edu/~javaplt/311/Readings/supplemental.pdf
