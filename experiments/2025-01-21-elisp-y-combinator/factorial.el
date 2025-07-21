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

;; CONTINUE meta-joke: Character analysis and factorial computation
(let* ((word "CONTINUE")
       (chars (string-to-list word))
       (ascii-sum (apply '+ chars))
       (char-product (apply '* (mapcar (lambda (c) (mod c 10)) chars))))
  (message "CONTINUE: length=%d, ASCII chars=%s, sum=%d, factorial of product mod 10: %d"
           (length word) chars ascii-sum (funcall fact-y char-product)))
;; Factorial Implementation:1 ends here
