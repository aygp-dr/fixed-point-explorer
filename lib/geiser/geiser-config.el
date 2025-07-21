;;; geiser-config.el --- Geiser configuration for fixed-point-explorer

;;; Commentary:
;; Configure Geiser for multi-implementation Scheme development

;;; Code:

(require 'geiser)

;; Implementation-specific settings
(setq geiser-guile-binary "guile3")
(setq geiser-chez-binary "chez-scheme")
(setq geiser-racket-binary "racket")

;; Load path configuration
(setq geiser-guile-load-path '("./src/guile" "./src/portable"))
(setq geiser-chez-load-path '("./src/chez" "./src/portable"))
(setq geiser-racket-load-path '("./src/racket" "./src/portable"))

;; Default to Guile for .scm files
(setq geiser-active-implementations '(guile chez racket))
(setq geiser-default-implementation 'guile)

;; Implementation detection based on file location
(defun fixed-point-explorer-geiser-impl ()
  "Detect Scheme implementation based on file path."
  (let ((file (buffer-file-name)))
    (cond
     ((string-match "/guile/" file) 'guile)
     ((string-match "/chez/" file) 'chez)
     ((string-match "/racket/" file) 'racket)
     (t geiser-default-implementation))))

(setq geiser-implementation-alist
      '(((regexp "\\.scm$") fixed-point-explorer-geiser-impl)
        ((regexp "\\.ss$") chez)
        ((regexp "\\.rkt$") racket)))

;; REPL settings
(setq geiser-repl-query-on-exit-p t)
(setq geiser-repl-history-filename "~/.geiser_history")
(setq geiser-repl-use-other-window t)

(provide 'geiser-config)
;;; geiser-config.el ends here
