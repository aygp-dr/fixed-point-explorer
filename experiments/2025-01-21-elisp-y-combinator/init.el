;; Configuration

;; Enable org-babel for elisp execution:


;; [[file:y-combinator.org::*Configuration][Configuration:1]]
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

;; Don't ask for confirmation on elisp blocks
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "emacs-lisp"))))
;; Configuration:1 ends here
