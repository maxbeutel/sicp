#lang racket

;; Resources:
;;
;; Video, although using backtracking:
;; https://www.youtube.com/watch?v=xFv_Hl4B83A (N Queens Problem using Backtracking by Abdul Bari)
;;
;; Links:
;; https://billthelizard.blogspot.com/2011/06/sicp-242-243-n-queens-problem.html
;; http://community.schemewiki.org/?sicp-ex-2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         ;; (2)
         ;; now filter the blown up set
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens) ;; a list of coordinates that are possible solutions for the previous k-1 columns
            ;; (1)
            ;; for each row in the k-th column
            ;; append this column to the previous solution
            ;; blowing up the set by factor "board-size"
            ;;
            ;; since we are blowing up the set, for each solution of k-1,
            ;; we create "board-size" new possible solutions
            ;; we need to flatten the result (hence the flatmap)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k ;; current column for which we create rows that need to be checked if safe
                    rest-of-queens))
                 (enumerate-interval 1 board-size))) ;; enumerate all rows for the given board size
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;;;

(define (flatmap proc sequence)
  (foldr append '() (map proc sequence)))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      '()
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define empty-board '())

(define (same-row P Q)
  (= (coordinate-row P) (coordinate-row Q)))

;; A line is defined as y = mx+c
;; Since we are only interested in diagonals, the only possible values for the gradient are m=1 or m=-1
;; So:
;;     y = (1) x + c      or
;;     y = (-1)x+c
;; So:
;;     c = y-c            or
;;     c = y+c
;;
;; Calculate c for both points P,Q and compoare the result. If they are on the same diagonal line, c will be equal
;; for both points.
(define (same-diagonal P Q)
  (define (calc-c op x y)
    (op x y))  
  (or (= (calc-c + (coordinate-row P) (coordinate-col P))
         (calc-c + (coordinate-row Q) (coordinate-col Q)))
      (= (calc-c - (coordinate-row P) (coordinate-col P))
         (calc-c - (coordinate-row Q) (coordinate-col Q)))))

(define (attackable? from to)
  (or (same-row from to) (same-diagonal from to)))

(define (safe? _ positions)
  (let ((test-coord (car positions))
        (ok-positions (cdr positions)))
    (= (length (filter (lambda (compare)
                         (not (attackable? compare test-coord)))
                       ok-positions))
       (length ok-positions))))

(define (adjoin-position row col sequence)
  (cons (make-coordinate row col) sequence))

(define (make-coordinate row col)
  (list row col))

(define (coordinate-row coord)
   (car coord))

(define (coordinate-col coord)
   (cadr coord))

;;;;;;

(length (queens 8))
(displayln "^ Total results")

