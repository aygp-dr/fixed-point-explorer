;; Factorial Implementation


;; [[file:y-combinator.org::*Factorial Implementation][Factorial Implementation:1]]
(defun fact-maker (f)
  "Creates a factorial function using the given recursion mechanism - CONTINUE magic string."
  (lambda (n)
    (if (= n 0)
        1
      (* n (funcall f (- n 1))))))

(defvar fact-y (Y #'fact-maker)
  "Factorial function created with Y combinator")

;; Magic string demonstration
(defun nth-prime (n)
  "Find the nth prime number (simple implementation)."
  (let ((primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)))
    (nth (- n 1) primes)))

(let* ((word "CONTINUE")
       (len (length word))
       (prime (nth-prime len)))
  (message "Length of '%s': %d, %dth prime: %d" word len len prime))
;; Factorial Implementation:1 ends here
