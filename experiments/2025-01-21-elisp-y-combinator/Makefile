# Makefile for Elisp Y Combinator Experiment

EMACS := emacs
ORG_FILE := y-combinator.org

.PHONY: help execute tangle detangle clean

help:
	@echo "Elisp Y Combinator Experiment"
	@echo "  make execute  - Execute all code blocks and show results"
	@echo "  make tangle   - Extract elisp files from org"
	@echo "  make detangle - Update org from elisp files"
	@echo "  make clean    - Remove tangled files"

execute:
	@echo "Executing all elisp blocks in $(ORG_FILE)..."
	@$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
		$(ORG_FILE) \
		-f org-babel-execute-buffer \
		-f save-buffer \
		--eval "(message \"Execution complete. Results saved.\")"

tangle:
	@echo "Tangling elisp code from $(ORG_FILE)..."
	@$(EMACS) --batch \
		--eval "(require 'org)" \
		$(ORG_FILE) \
		-f org-babel-tangle \
		--eval "(message \"Tangled files: %s\" (org-babel-tangle))"
	@echo "Tangled files:"
	@ls -la *.el 2>/dev/null || echo "No elisp files found"

detangle:
	@echo "Detangling (updating org from elisp files)..."
	@$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-babel-detangle-jump-to-org t)" \
		$(ORG_FILE) \
		-f org-babel-detangle

clean:
	@echo "Removing tangled elisp files..."
	@rm -f *.el
	@echo "Cleaned."