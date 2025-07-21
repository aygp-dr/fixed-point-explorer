# Testing Guidelines for Elisp Org-Mode Experiments

## Interactive Execution with tmux

For org-mode files with elisp code blocks that need interactive execution:

### 1. Start tmux session
```bash
tmux new-session -d -s elisp-test "emacs -nw y-combinator.org"
```

### 2. Execute all blocks
```bash
tmux send-keys -t elisp-test "M-x org-babel-execute-buffer" Enter
```

### 3. Connect to session to view results
```bash
tmux attach -t elisp-test
```

### 4. Save and exit when done
```bash
# In Emacs:
C-x C-s  # Save
C-x C-c  # Exit
```

### 5. Clean up session
```bash
tmux kill-session -t elisp-test
```

## Makefile Integration

Add tmux-based testing to Makefile:

```makefile
test-interactive:
	@echo "Starting interactive Emacs session..."
	@tmux new-session -d -s elisp-test "emacs -nw $(ORG_FILE)"
	@tmux send-keys -t elisp-test "M-x org-babel-execute-buffer" Enter
	@echo "Session started. Connect with: tmux attach -t elisp-test"

connect:
	@tmux attach -t elisp-test || echo "No elisp-test session found"

cleanup:
	@tmux kill-session -t elisp-test 2>/dev/null || echo "No session to kill"
```

## Benefits

- Non-blocking execution
- Full interactive Emacs environment
- Results visible in RESULTS drawers
- Can inspect and debug interactively
- Session persists for manual testing