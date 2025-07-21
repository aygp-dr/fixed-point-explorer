# Observation: 2025-07-21 - Elisp Y Combinator Experiment Plan

## Concept
Create an org-mode file with Y combinator implementations in Emacs Lisp, with inline executable code blocks.

## Structure
- Single large org file with elisp code blocks
- Each block executable with C-c C-c
- Examples include fibonacci, factorial, list operations
- Makefile target to execute all blocks programmatically

## Key Elements
1. Y combinator in elisp syntax
2. Inline org-babel execution
3. Results displayed in-buffer
4. Automated execution via Make

## For Builder
This would create:
- `experiments/2025-01-21-elisp-y-combinator/y-combinator.org`
- Makefile target: `gmake test-elisp-org`
- All examples return expected results (fib 10 = 55, etc.)

## Example Structure
```org
#+TITLE: Y Combinator in Emacs Lisp

* Y Combinator Definition
#+BEGIN_SRC elisp :results value
(defun Y (f)
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))
#+END_SRC

* Fibonacci Example  
#+BEGIN_SRC elisp :results value
(funcall (Y (lambda (f)
              (lambda (n)
                (if (< n 2)
                    n
                  (+ (funcall f (- n 1))
                     (funcall f (- n 2)))))))
         10)
#+END_SRC

#+RESULTS:
: 55
```