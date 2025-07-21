;; Y Combinator Definition

;; The Y combinator in Emacs Lisp:


;; [[file:y-combinator.org::*Y Combinator Definition][Y Combinator Definition:1]]
(defun Y (f)
  "The Y combinator - creates recursive functions from non-recursive ones."
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))

;; CONTINUE meta-joke: Y combinator applied to word analysis  
(let* ((word "CONTINUE")
       (chars (string-to-list word))
       (bit-pattern (mapcar (lambda (c) (mod c 2)) chars))
       (recursive-sum ((Y (lambda (f) 
                            (lambda (lst acc)
                              (if (null lst) acc
                                (funcall f (cdr lst) (+ acc (car lst)))))))
                       bit-pattern 0)))
  (message "CONTINUE: Y-combinator recursive bit sum of '%s' = %d (AI programming continues...)"
           word recursive-sum))
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

;; Social Media Summary: "Org-Mode + Elisp = Literate Programming Magic"

;; 🧙‍♂️ **Just discovered the power of bidirectional literate programming with Org-Mode + Elisp!**

;; 📝 **The workflow:**
;; • Write elisp code in beautiful org-mode documents  
;; • Execute blocks inline (C-c C-c) for instant feedback
;; • Tangle to .el files (C-c C-v t) for proper elisp development
;; • Edit source files when debugging
;; • Detangle back to org - your changes sync automatically! 

;; 🔗 **The magic:** Special comment markers create bidirectional links between your org prose and generated source code.

;; ⚡ **Live example:** Implemented Y combinator in elisp with:

;; [[file:y-combinator.org::*Social Media Summary: "Org-Mode + Elisp = Literate Programming Magic"][Social Media Summary: "Org-Mode + Elisp = Literate Programming Magic":1]]
;; Create fibonacci without explicit recursion!
(defvar fib (Y (lambda (f) 
                 (lambda (n) 
                   (if (< n 2) n 
                     (+ (funcall f (- n 1)) 
                        (funcall f (- n 2))))))))
(funcall fib 10) ; => 55
;; Social Media Summary: "Org-Mode + Elisp = Literate Programming Magic":1 ends here

;; Mutual Recursion Clarification

;; The mutual recursion section uses **two separate Y combinators** with closure capture, not a single Y combinator handling multiple functions. This is a creative workaround for Y's single-recursion nature:


;; [[file:y-combinator.org::*Mutual Recursion Clarification][Mutual Recursion Clarification:1]]
;; Two Y combinators working together
(setq even-y (Y (lambda (f) (lambda (n) (if (= n 0) t (funcall odd-y (- n 1)))))))
(setq odd-y  (Y (lambda (f) (lambda (n) (if (= n 0) nil (funcall even-y (- n 1)))))))
;; Mutual Recursion Clarification:1 ends here
