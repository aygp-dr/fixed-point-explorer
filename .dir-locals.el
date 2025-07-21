;;; Directory Local Variables for fixed-point-explorer
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   ;; Add project root to load path
                   (add-to-list 'load-path (expand-file-name "lib/geiser" (projectile-project-root)))
                   ;; Configure Geiser for multiple Scheme implementations
                   (require 'geiser-mode nil t)
                   (require 'geiser-guile nil t)
                   (require 'geiser-chez nil t)
                   (require 'geiser-racket nil t)))))
 (scheme-mode . ((geiser-active-implementations . (guile chez racket))
                 (geiser-default-implementation . guile)
                 (geiser-guile-binary . "guile3")
                 (geiser-chez-binary . "chez-scheme")
                 (geiser-racket-binary . "racket"))))
