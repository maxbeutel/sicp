#lang racket

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 30)

(withdraw 10000)

;;;;;;;;;;;;;;;;;;;;

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 95)

(new-withdraw 15)

;;;;;;;;;;;;;;;;;;;;

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))

(define W2 (make-withdraw 100))

(W1 50)

(W2 70)

;;;;;;;;;;;;;;;;;;;;

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request" m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
