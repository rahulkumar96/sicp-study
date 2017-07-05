;; SICP 1.12

;; Exercise 1.12 The following pattern of numbers is called Pascal's triangle.
;;
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. Write
;; a procedure that computes elements of Pascal's triangle by means of
;; a recursive process.

;; ANSWER ------------------------------------------------------------

(define (pascal-element x y)
  (define (edge?)
    (or (= x y) (= y 0)))
  (cond ((edge?) 1)
        (else (+ (pascal-element (- x 1) (- y 1))
         (pascal-element (- x 1) y)))))


