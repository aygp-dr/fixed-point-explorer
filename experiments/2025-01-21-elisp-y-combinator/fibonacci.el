;; Fibonacci Implementation


;; [[file:y-combinator.org::*Fibonacci Implementation][Fibonacci Implementation:1]]
(defun fib-maker (f)
  "Creates a fibonacci function using the given recursion mechanism."
  (lambda (n)
    (if (< n 2)
        n
      (+ (funcall f (- n 1))
         (funcall f (- n 2))))))

;; Create fibonacci using Y combinator
(defvar fib-y (Y #'fib-maker)
  "Fibonacci function created with Y combinator")
;; Fibonacci Implementation:1 ends here
