;; Test script to debug detangle functionality
(require 'org)
(require 'ob-tangle)

(defun debug-detangle-process (file)
  "Debug the detangle process step by step."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (message "=== Debugging detangle process for %s ===" file)
    (message "org-link-bracket-re: %s" org-link-bracket-re)
    
    ;; Step 1: Look for org-link-bracket-re matches
    (let ((counter 0) new-body end)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (message "Found link match: '%s'" (match-string 0))
        (message "Match groups: 1='%s' 2='%s'" (match-string 1) (match-string 2))
        
        ;; Step 2: Look for corresponding end marker
        (if (and (match-string 2)
                 (re-search-forward
                  (concat " " (regexp-quote (match-string 2)) " ends here") nil t))
            (progn
              (message "Found end marker: '%s'" (match-string 0))
              (setq end (match-end 0))
              (forward-line -1)
              (message "Would attempt to detangle block")
              (setq counter (+ 1 counter)))
          (progn
            (message "No matching end marker found")
            (setq end (point))))
        (goto-char end))
      (message "Total detangleable blocks found: %d" counter))))

;; Test the function
(debug-detangle-process "factorial.el")