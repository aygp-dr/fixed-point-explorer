;; Ackermann Function


;; [[file:y-combinator.org::*Ackermann Function][Ackermann Function:1]]
(defun ackermann-maker (f)
  "Creates Ackermann function using the given recursion mechanism."
  (lambda (m n)
    (cond ((= m 0) (+ n 1))
          ((= n 0) (funcall f (- m 1) 1))
          (t (funcall f (- m 1) (funcall f m (- n 1)))))))

(defvar ackermann-y (Y #'ackermann-maker)
  "Ackermann function created with Y combinator")
;; Ackermann Function:1 ends here

;; Mutual Recursion (Even/Odd)

;; This requires a different approach since Y combinator handles single recursion:


;; [[file:y-combinator.org::*Mutual Recursion (Even/Odd)][Mutual Recursion (Even/Odd):1]]
(defun even-odd-maker ()
  "Creates mutually recursive even/odd checkers."
  (let* ((even-y nil)
         (odd-y nil))
    (setq even-y 
          (Y (lambda (f)
               (lambda (n)
                 (if (= n 0)
                     t
                   (funcall odd-y (- n 1)))))))
    (setq odd-y
          (Y (lambda (f)
               (lambda (n)
                 (if (= n 0)
                     nil
                   (funcall even-y (- n 1)))))))
    (list even-y odd-y)))

(defvar even-odd-pair (even-odd-maker))
(defvar even-y (car even-odd-pair))
(defvar odd-y (cadr even-odd-pair))

;; CONTINUE meta-joke: Advanced computations with ASCII values
(let* ((word "CONTINUE")
       (ascii-chars (mapcar (lambda (c) c) (string-to-list word)))
       (sum-ascii (apply '+ ascii-chars))
       (even-count (length (seq-filter (lambda (c) (funcall even-y c)) ascii-chars)))
       (ackermann-val (funcall ackermann-y 1 (mod sum-ascii 5))))
  (message "CONTINUE: ASCII sum=%d, even chars=%d, Ackermann(1,%d)=%d, all Y combinators!"
           sum-ascii even-count (mod sum-ascii 5) ackermann-val))
;; Mutual Recursion (Even/Odd):1 ends here
