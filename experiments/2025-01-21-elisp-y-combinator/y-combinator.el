;; Y Combinator Definition

;; The Y combinator in Emacs Lisp:


;; [[file:y-combinator.org::*Y Combinator Definition][Y Combinator Definition:1]]
(defun Y (f)
  "The Y combinator - creates recursive functions from non-recursive ones."
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))
;; Y Combinator Definition:1 ends here

;; Test Fibonacci


;; [[file:y-combinator.org::*Test Fibonacci][Test Fibonacci:1]]
(funcall fib-y 0)
;; Test Fibonacci:1 ends here



;; #+RESULTS:
;; : 0


;; [[file:y-combinator.org::*Test Fibonacci][Test Fibonacci:2]]
(funcall fib-y 1)
;; Test Fibonacci:2 ends here



;; #+RESULTS:
;; : 1


;; [[file:y-combinator.org::*Test Fibonacci][Test Fibonacci:3]]
(funcall fib-y 5)
;; Test Fibonacci:3 ends here



;; #+RESULTS:
;; : 5


;; [[file:y-combinator.org::*Test Fibonacci][Test Fibonacci:4]]
(funcall fib-y 10)
;; Test Fibonacci:4 ends here



;; #+RESULTS:
;; : 55


;; [[file:y-combinator.org::*Test Fibonacci][Test Fibonacci:5]]
(mapcar fib-y '(0 1 2 3 4 5 6 7 8 9 10))
;; Test Fibonacci:5 ends here

;; Test Factorial


;; [[file:y-combinator.org::*Test Factorial][Test Factorial:1]]
(funcall fact-y 5)
;; Test Factorial:1 ends here



;; #+RESULTS:
;; : 120


;; [[file:y-combinator.org::*Test Factorial][Test Factorial:2]]
(mapcar fact-y '(0 1 2 3 4 5 6))
;; Test Factorial:2 ends here



;; #+RESULTS:
;; : length-y


;; [[file:y-combinator.org::*Length][Length:2]]
(funcall length-y '(a b c d e))
;; Length:2 ends here



;; #+RESULTS:
;; : append-y


;; [[file:y-combinator.org::*Append][Append:2]]
(funcall append-y '(1 2 3) '(4 5 6))
;; Append:2 ends here



;; #+RESULTS:
;; : map-y


;; [[file:y-combinator.org::*Map][Map:2]]
(funcall map-y (lambda (x) (* x 2)) '(1 2 3 4 5))
;; Map:2 ends here



;; #+RESULTS:
;; : filter-y


;; [[file:y-combinator.org::*Filter][Filter:2]]
(funcall filter-y #'evenp '(1 2 3 4 5 6))
;; Filter:2 ends here



;; #+RESULTS:
;; : ackermann-y


;; [[file:y-combinator.org::*Ackermann Function][Ackermann Function:2]]
(funcall ackermann-y 3 3)
;; Ackermann Function:2 ends here



;; #+RESULTS:
;; : odd-y


;; [[file:y-combinator.org::*Mutual Recursion (Even/Odd)][Mutual Recursion (Even/Odd):2]]
(list (funcall even-y 10) (funcall odd-y 10))
;; Mutual Recursion (Even/Odd):2 ends here

;; Performance Comparison

;; Compare Y combinator version with standard recursion:


;; [[file:y-combinator.org::*Performance Comparison][Performance Comparison:1]]
(defun fib-standard (n)
  "Standard recursive fibonacci."
  (if (< n 2)
      n
    (+ (fib-standard (- n 1))
       (fib-standard (- n 2)))))

;; Time comparison
(let ((n 20))
  (list
   (cons "Y-combinator"
         (benchmark-run 100 (funcall fib-y n)))
   (cons "Standard"
         (benchmark-run 100 (fib-standard n)))))
;; Performance Comparison:1 ends here
