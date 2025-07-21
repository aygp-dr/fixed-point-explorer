;; Configuration for Seamless Elisp Development

;; [[file:y-combinator.org::*Configuration for Seamless Elisp Development][Configuration for Seamless Elisp Development:1]]
;; Essential org-babel elisp setup
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

;; Never ask for confirmation (security consideration: only for trusted code)
(setq org-confirm-babel-evaluate nil)

;; Auto-tangle on save
(add-hook 'org-mode-hook
          (lambda () 
            (add-hook 'after-save-hook #'org-babel-tangle nil t)))
;; Configuration for Seamless Elisp Development:1 ends here
