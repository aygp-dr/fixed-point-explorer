;;; init.el --- Fixed Point Explorer implementation configuration
;;; For working with the actual codebase after tangling

;; Set up package.el
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Ensure packages are installed
(defun ensure-package-installed (&rest packages)
  "Ensure PACKAGES are installed."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (unless (assoc package package-archive-contents)
         (package-refresh-contents))
       (package-install package)))
   packages))

;; Install required packages
(ensure-package-installed 
 'projectile 
 'geiser
 'geiser-guile 
 'geiser-chez 
 'geiser-racket
 'magit)

;; Configure projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Set project root
(defvar fixed-point-explorer-root 
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of Fixed Point Explorer project.")

;; Configure Geiser
(require 'geiser)
(require 'geiser-guile nil t)
(require 'geiser-chez nil t)
(require 'geiser-racket nil t)

;; Set Geiser implementation paths
(setq geiser-active-implementations '(guile chez racket))
(setq geiser-default-implementation 'guile)

;; Set binary paths
(setq geiser-guile-binary 
      (or (executable-find "guile3")
          (executable-find "guile")
          "guile"))
(setq geiser-chez-binary 
      (or (executable-find "chez-scheme")
          (executable-find "scheme")
          "chez-scheme"))
(setq geiser-racket-binary 
      (or (executable-find "racket")
          "racket"))

;; Set load paths
(setq geiser-guile-load-path 
      (list (expand-file-name "src/guile" fixed-point-explorer-root)
            (expand-file-name "src/portable" fixed-point-explorer-root)))
(setq geiser-chez-load-path 
      (list (expand-file-name "src/chez" fixed-point-explorer-root)
            (expand-file-name "src/portable" fixed-point-explorer-root)))
(setq geiser-racket-load-path 
      (list (expand-file-name "src/racket" fixed-point-explorer-root)
            (expand-file-name "src/portable" fixed-point-explorer-root)))

;; Lean configuration
(let ((lean-bin (expand-file-name "tools/formal-methods/lean4/bin/lean" 
                                  fixed-point-explorer-root)))
  (when (file-executable-p lean-bin)
    (setq lean4-executable lean-bin)
    (setq lean4-lake-executable 
          (expand-file-name "tools/formal-methods/lean4/bin/lake" 
                            fixed-point-explorer-root))))

;; Quick access to implementation files
(defun fpe-find-ycombinator ()
  "Open y-combinator.scm."
  (interactive)
  (find-file (expand-file-name "src/portable/y-combinator.scm" fixed-point-explorer-root)))

(defun fpe-find-fibonacci ()
  "Open fibonacci.scm."
  (interactive)
  (find-file (expand-file-name "src/portable/fibonacci.scm" fixed-point-explorer-root)))

(defun fpe-find-list-ops ()
  "Open list-ops.scm."
  (interactive)
  (find-file (expand-file-name "src/portable/list-ops.scm" fixed-point-explorer-root)))

(defun fpe-find-lean-specs ()
  "Open Lean specifications directory."
  (interactive)
  (dired (expand-file-name "docs/specs/lean" fixed-point-explorer-root)))

;; Testing commands
(defun fpe-run-tests ()
  "Run all tests."
  (interactive)
  (let ((default-directory fixed-point-explorer-root))
    (compile "make test")))

(defun fpe-run-guile-tests ()
  "Run Guile tests."
  (interactive)
  (let ((default-directory fixed-point-explorer-root))
    (compile "make test-guile")))

(defun fpe-run-lean-tests ()
  "Run Lean tests."
  (interactive)
  (let ((default-directory (expand-file-name "docs/specs/lean" fixed-point-explorer-root)))
    (compile "./build.sh")))

;; REPL commands
(defun fpe-run-guile ()
  "Start Guile REPL with project settings."
  (interactive)
  (let ((default-directory fixed-point-explorer-root))
    (run-geiser 'guile)))

(defun fpe-run-chez ()
  "Start Chez REPL with project settings."
  (interactive)
  (let ((default-directory fixed-point-explorer-root))
    (run-geiser 'chez)))

(defun fpe-run-racket ()
  "Start Racket REPL with project settings."
  (interactive)
  (let ((default-directory fixed-point-explorer-root))
    (run-geiser 'racket)))

;; Load and test specific implementations
(defun fpe-load-and-test-fib ()
  "Load fibonacci and run tests in REPL."
  (interactive)
  (fpe-run-guile)
  (geiser-load-file (expand-file-name "src/portable/fibonacci.scm" fixed-point-explorer-root))
  (geiser-eval-definition "(test-fibonacci)"))

;; Key bindings
(global-set-key (kbd "C-c f y") 'fpe-find-ycombinator)
(global-set-key (kbd "C-c f f") 'fpe-find-fibonacci)
(global-set-key (kbd "C-c f l") 'fpe-find-list-ops)
(global-set-key (kbd "C-c f L") 'fpe-find-lean-specs)

(global-set-key (kbd "C-c f t") 'fpe-run-tests)
(global-set-key (kbd "C-c f T") 'fpe-run-lean-tests)

(global-set-key (kbd "C-c f g") 'fpe-run-guile)
(global-set-key (kbd "C-c f c") 'fpe-run-chez)
(global-set-key (kbd "C-c f r") 'fpe-run-racket)

(global-set-key (kbd "C-c f d") 
                (lambda () 
                  (interactive) 
                  (dired fixed-point-explorer-root)))

(global-set-key (kbd "C-c f m") 'magit-status)

;; Hydra for quick access (if available)
(when (fboundp 'defhydra)
  (defhydra fpe-hydra (:color blue :hint nil)
    "
Fixed Point Explorer:
Files: _y_: y-combinator  _f_: fibonacci  _l_: list-ops  _L_: lean specs
Tests: _t_: all tests     _T_: lean tests
REPLs: _g_: guile        _c_: chez       _r_: racket
Other: _d_: dired        _m_: magit      _q_: quit
"
    ("y" fpe-find-ycombinator)
    ("f" fpe-find-fibonacci)
    ("l" fpe-find-list-ops)
    ("L" fpe-find-lean-specs)
    ("t" fpe-run-tests)
    ("T" fpe-run-lean-tests)
    ("g" fpe-run-guile)
    ("c" fpe-run-chez)
    ("r" fpe-run-racket)
    ("d" (dired fixed-point-explorer-root))
    ("m" magit-status)
    ("q" nil))
  
  (global-set-key (kbd "C-c f h") 'fpe-hydra/body))

;; Emacs 30+ specific features
(when (>= emacs-major-version 30)
  ;; Use built-in tree-sitter if available
  (when (treesit-available-p)
    (setq treesit-language-source-alist
          '((scheme "https://github.com/6cdh/tree-sitter-scheme"))))
  
  ;; Enable new completion features
  (setq completion-auto-help 'visible)
  (setq completion-auto-select 'second-tab))

;; Display startup info
(message "Fixed Point Explorer Implementation Mode")
(message "Project root: %s" fixed-point-explorer-root)
(message "Quick keys: C-c f h (hydra menu) or C-c f [y/f/l/t/g]")

;; Check implementations
(let ((missing '())
      (found '()))
  (if (executable-find geiser-guile-binary)
      (push "guile3" found)
    (push "guile3" missing))
  (if (executable-find geiser-chez-binary)
      (push "chez" found)
    (push "chez-scheme" missing))
  (if (executable-find geiser-racket-binary)
      (push "racket" found)
    (push "racket" missing))
  (message "Scheme: Found %s%s" 
           (mapconcat 'identity found ", ")
           (if missing (format " | Missing: %s" (mapconcat 'identity missing ", ")) "")))

;; Set default directory
(setq default-directory fixed-point-explorer-root)

;; Enable useful modes
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)

;; If no file specified, show project overview
(when (= (length command-line-args) 1)
  (dired fixed-point-explorer-root))

;;; init.el ends here
