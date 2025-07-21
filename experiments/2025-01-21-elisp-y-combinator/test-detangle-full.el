;; Test full detangle functionality
(require 'org)
(require 'ob-tangle)

(defun test-full-detangle (file)
  "Test the complete detangle process."
  (message "=== Testing full detangle on %s ===" file)
  
  ;; First, let's see what org-babel-tangle-jump-to-org returns
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    ;; Find the first tangle block
    (when (re-search-forward org-link-bracket-re nil t)
      (when (and (match-string 2)
                 (re-search-forward
                  (concat " " (regexp-quote (match-string 2)) " ends here") nil t))
        (forward-line -1)
        (message "Current position: %d" (point))
        (message "Current line: '%s'" (thing-at-point 'line t))
        
        ;; Try to jump to org and get the body
        (condition-case err
            (let ((new-body (org-babel-tangle-jump-to-org)))
              (if new-body
                  (message "Successfully retrieved body from org: %s chars" (length new-body))
                (message "org-babel-tangle-jump-to-org returned nil")))
          (error (message "Error in org-babel-tangle-jump-to-org: %s" err))))))
  
  ;; Now test the actual detangle
  (condition-case err
      (let ((result (org-babel-detangle file)))
        (message "org-babel-detangle returned: %d" result))
    (error (message "Error in org-babel-detangle: %s" err))))

(test-full-detangle "factorial.el")