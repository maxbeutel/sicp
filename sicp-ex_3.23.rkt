#lang sicp

;; Node of a double-ended queue.
(define (node-new prev val next) (list prev val next))

(define (node-prev n) (car n))
(define (node-val n) (cadr n))
(define (node-next n) (caddr n))
(define (node-prev? n) (not (null? (node-prev n))))
(define (node-next? n) (not (null? (node-next n))))

(define (node-set-prev! n val) (set-car! n val))
(define (node-set-next! n val) (set-car! (cddr n) val))

(define (node-dump n)
  (display "\t < ")
  (display (if (node-prev? n) (node-val (node-prev n)) 0))
  (display " | ")
  (display (node-val n))
  (display " | ")
  (display (if (node-next? n) (node-val (node-next n)) 0))
  (display " >\n"))

;; Double-ended queue.
(define (dequeue-new)
  (cons '() '()))

(define (dequeue-init-empty d val)
  (let ((node (node-new '() val '())))
    (begin (dequeue-set-front-ptr! d node)
           (dequeue-set-rear-ptr! d node))))

(define (dequeue-front-ptr d) (car d))
(define (dequeue-rear-ptr d) (cdr d))
(define (dequeue-set-front-ptr! d node) (set-car! d node))
(define (dequeue-set-rear-ptr! d node) (set-cdr! d node))

(define (dequeue-empty? d) (null? (dequeue-front-ptr d)))

(define (dequeue-front-eq-rear? d) (equal? (dequeue-front-ptr d) (dequeue-rear-ptr d)))

(define (dequeue-reset-ptrs d)
  (begin (dequeue-set-front-ptr! d '())
         (dequeue-set-rear-ptr! d '())))

(define (dequeue-insert-front! d val)
  (if (dequeue-empty? d)
      (dequeue-init-empty d val)
      (let ((node (node-new '() val (dequeue-front-ptr d))))
        (begin (dequeue-set-front-ptr! d node)
               (node-set-prev! (node-next (dequeue-front-ptr d)) (dequeue-front-ptr d))))))

(define (dequeue-insert-rear! d val)
  (if (dequeue-empty? d)
      (dequeue-init-empty d val)
      (let ((node (node-new (dequeue-rear-ptr d) val '())))
        (begin (dequeue-set-rear-ptr! d node)
               (node-set-next! (node-prev (dequeue-rear-ptr d)) (dequeue-rear-ptr d))))))

(define (dequeue-delete-front! d)
  (cond ((dequeue-empty? d) (error "Can't delete from empty queue."))
        ((dequeue-front-eq-rear? d) (dequeue-reset-ptrs d))
        (else (begin (dequeue-set-front-ptr! d (node-next (dequeue-front-ptr d)))
                     (node-set-prev! (dequeue-front-ptr d) '())))))

(define (dequeue-delete-rear! d)
  (cond ((dequeue-empty? d) (error "Can't delete from empty queue."))
        ((dequeue-front-eq-rear? d) (dequeue-reset-ptrs d))
        (else (begin
                (dequeue-set-rear-ptr! d (node-prev (dequeue-rear-ptr d)))
                (node-set-next! (dequeue-rear-ptr d) '())))))

(define (dequeue-dump d)
  (define (dequeue-dump-items n)
    (if (null? n)
        (display "")
        (begin (node-dump n)
               (dequeue-dump-items (node-next n)))))
  (if (dequeue-empty? d)
      (display "[ Queue is empty ]\n")
      (begin (display "[ Queue dump ]\n")
             (display "Front")
             (node-dump (dequeue-front-ptr d))
             (display "Rear")
             (node-dump (dequeue-rear-ptr d))
             (display "Items\n")
             (dequeue-dump-items [dequeue-front-ptr d])
             (display "-----------------------\n"))))

;;;;;;;;;;;;;;;;;;;;


(define d (dequeue-new))
(dequeue-dump d)

(dequeue-insert-front! d 1)
(dequeue-dump d)

(dequeue-insert-front! d 2)
(dequeue-dump d)

(dequeue-insert-front! d 3)
(dequeue-dump d)

(dequeue-delete-front! d)
(dequeue-dump d)

(dequeue-delete-front! d)
(dequeue-dump d)

(dequeue-delete-front! d)
(dequeue-dump d)

(dequeue-insert-rear! d 1)
(dequeue-dump d)

(dequeue-insert-rear! d 2)
(dequeue-dump d)

(dequeue-insert-rear! d 3)
(dequeue-dump d)

(dequeue-delete-rear! d)
(dequeue-dump d)

(dequeue-delete-rear! d)
(dequeue-dump d)

(dequeue-delete-rear! d)
(dequeue-dump d)
