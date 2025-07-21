;; Factorial Implementation


;; [[file:y-combinator.org::*Factorial Implementation][Factorial Implementation:1]]
(defun fact-maker (f)
  "Creates a factorial function using the given recursion mechanism."
  (lambda (n)
    (if (= n 0)
        1
      (* n (funcall f (- n 1))))))

(defvar fact-y (Y #'fact-maker)
  "Factorial function created with Y combinator")
;; Factorial Implementation:1 ends here
