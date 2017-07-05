;; Exercise 1.11.  A function f is defined by the rule that f(n) = n
;; if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a
;; procedure that computes f by means of a recursive process. Write a
;; procedure that computes f by means of an iterative process.

;; First, the recursive version.

(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))) )))

;; Now the iterative version.

(define (f-iter n)
  (define (f-iter-helper a b c n)
    (cond ((< n 3) n)
          ((= n 3) c)
          (else (f-iter-helper b c (+ c (* 2 b) (* 3 a)) (- n 1)))))
  (f-iter-helper 1 2 4 n))

