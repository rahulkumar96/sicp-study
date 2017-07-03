;; SICP 1.8

;; Exercise 1.8.  Newton's method for cube roots is based on the fact
;; that if y is an approximation to the cube root of x, then a better
;; approximation is given by the value (image omitted).
;;
;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure. (In section 1.3.4 we will see how to
;; implement Newton's method in general as an abstraction of these
;; square-root and cube-root procedures.)

;; ANSWER ------------------------------------------------------------

;; Similar to Ex 1.7

(define (cube-root x)
  (cube-root-loop x 1.0))

(define (cube-root-loop x guess)
  (if (> x 1)
      (if (good-enough-large? x guess)
          guess
          (cube-root-loop x (improve-guess x guess)))
      
      (if (good-enough-small? x guess)
          guess
          (cube-root-loop x (improve-guess x guess)))))
  
(define (good-enough-large? x guess)
  (> 0.01 (abs(- (/ x (square guess)) guess))))

(define (good-enough-small? x guess)
  (> (/ guess 1000000) (abs(- (cube guess) x))))

(define (improve-guess x guess)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube x) (* x x x))    
(define (square x) (* x x))

