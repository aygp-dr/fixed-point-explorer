;; Enhanced Error Handling


;; [[file:y-combinator.org::*Enhanced Error Handling][Enhanced Error Handling:1]]
(defun Y-safe (f max-depth)
  "Y combinator with recursion depth limit for safety."
  (let ((depth 0))
    ((lambda (x) 
       (funcall f (lambda (y) 
                    (if (> (setq depth (1+ depth)) max-depth)
                        (error "Max recursion depth %d exceeded" max-depth)
                      (prog1 (funcall (funcall x x) y)
                        (setq depth (1- depth)))))))
     (lambda (x) 
       (funcall f (lambda (y) 
                    (if (> (setq depth (1+ depth)) max-depth)
                        (error "Max recursion depth %d exceeded" max-depth)
                      (prog1 (funcall (funcall x x) y)
                        (setq depth (1- depth))))))))))

;; Safe fibonacci with depth limit
(defvar fib-safe (Y-safe #'fib-maker 100)
  "Fibonacci with stack overflow protection")

;; CONTINUE meta-joke: Safe Y combinator with character encoding
(let* ((word "CONTINUE")
       (hex-values (mapcar (lambda (c) (format "%02x" c)) (string-to-list word)))
       (safe-depth (length word))
       (safe-result (condition-case err
                        (funcall (Y-safe (lambda (f) 
                                           (lambda (n) (if (<= n 1) n (+ n (funcall f (- n 1))))))
                                         safe-depth)
                                 (length hex-values))
                      (error (format "Error: %s" err)))))
  (message "CONTINUE: hex=%s, safe Y-depth=%d, result=%s (programming safety first!)"
           hex-values safe-depth safe-result))
;; Enhanced Error Handling:1 ends here
