;; Practical Detangle Example

;; This demonstrates the complete round-trip editing workflow:

;; 1. **Original tangle:** This block creates =detangle-demo.el=

;; [[file:y-combinator.org::*Practical Detangle Example][Practical Detangle Example:1]]
(defun demo-function (msg)
  "Modified version from source file - DETANGLE TEST"
  (message "Source says: %s [MODIFIED]" msg))
;; Practical Detangle Example:1 ends here
