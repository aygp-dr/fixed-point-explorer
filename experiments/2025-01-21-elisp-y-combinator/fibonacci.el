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

;; CONTINUE meta-joke: ASCII sum computation
(let* ((word "CONTINUE")
       (ascii-values (mapcar (lambda (c) (char-to-string c)) word))
       (ascii-sum (apply '+ (mapcar (lambda (c) (string-to-char c)) ascii-values))))
  (message "CONTINUE: %d chars, ASCII sum: %d, Fibonacci of sum mod 20: %d" 
           (length word) ascii-sum (funcall fib-y (mod ascii-sum 20))))
;; Fibonacci Implementation:1 ends here
