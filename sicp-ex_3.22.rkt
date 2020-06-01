#lang sicp

;;;;;;;;;;;;;;;;

(define (print-list L)
  (cond ((null? L)
         (display "------\n"))
        (else
         (begin
           (display (car L))
           (display "\n")
           (print-list (cdr L))))))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((null? front-ptr)
               (begin
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)))
              (else
               (begin
                 (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair))))))
    (define (delete)
      (set! front-ptr (cdr front-ptr)))
    (define (print)
      (print-list front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            ((eq? m 'print) print)
            (else (error "Unknown request" m))))
    dispatch))

(define q (make-queue))
((q 'print))

((q 'insert) 'a)
((q 'print))

((q 'insert) 'b)
((q 'print))

((q 'delete))
((q 'print))

((q 'delete))
((q 'print))
