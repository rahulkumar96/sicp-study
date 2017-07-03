;; SICP 1.7

;; Exercise 1.7.  The good-enough? test used in computing square roots
;; will not be very effective for finding the square roots of very
;; small numbers. Also, in real computers, arithmetic operations are
;; almost always performed with limited precision. This makes our test
;; inadequate for very large numbers. Explain these statements, with
;; examples showing how the test fails for small and large numbers. An
;; alternative strategy for implementing good-enough? is to watch how
;; guess changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better
;; for small and large numbers?

;; ANSWER ------------------------------------------------------------

;; For small numbers the good-enough? test will fail because it will return
;; true as soon as the (guess^2 - x) is less than the relative error specified,
;; even though it might not be close to the actual result. Eg. for 1e-20 it returns
;; 0.03125 but the correct answer is 1e-10.
;; 
;; The case for big numbers is a bit different. The precision is limited when performing
;; arithmetic operations on large numbers, eg. one cannot subtract 0.001 from 
;; say 95453645569655557844556846841 and expect to get the correct result.
;; There is a margin for error called epsilon defined for floating point precision,
;; if that epsilon exceeds the relative error (0.001 here) it might happen that for huge 
;; numbers the result will never converge and the program can get in a state of infinite
;; loop. So it might happen that while converging the round off leads to a state which takes
;; the guess to a value is never quite close enough to get the result.

;; Both of these problems can be dealt with if we check for relative change in guess
;; compared to a small fraction of the guess.
;; Checking against variable tolerance will solve the small numbers issue and checking for 
;; change in guess w.r.t to previous guess will solve the infinite loop issue.
;; However if we use them together, then in case of large numbers the tolerance may be 
;; more than what we may desire, so we can choose to just check the relative guesses and still
;; keep a fixed tolerance.

;; Code to handle both the cases:

(define (root x)
  (sqrt-loop x 1.0))

(define (sqrt-loop x guess)
  (if (> x 1)
      (if (good-enough-large? x guess)
          guess
          (sqrt-loop x (improve-guess x guess)))
      
      (if (good-enough-small? x guess)
          guess
          (sqrt-loop x (improve-guess x guess)))))
  
(define (good-enough-large? x guess)
  (> 0.01 (abs(- (/ x guess) guess))))

(define (good-enough-small? x guess)
  (> (/ guess 1000000) (abs(- (square guess) x))))

(define (improve-guess x guess)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (square x) (* x x))
