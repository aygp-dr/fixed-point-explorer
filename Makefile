# Makefile for fixed-point-explorer
# Default target is help
.DEFAULT_GOAL := help

# Use gmake explicitly
MAKE := gmake

.PHONY: help all run run-lean run-all test test-guile test-chez test-racket clean deps setup lean-tools
.PHONY: repl-guile repl-chez repl-racket benchmark test-guile-extra test-chez-extra
.PHONY: lean-version install-deps test-integration test-integration-guile
.PHONY: test-lean-fibonacci test-lean-ycombinator test-lean-properties test-lean-all test-lean-interactive

# Scheme implementations
GUILE := guile3
CHEZ := chez-scheme
RACKET := racket

# Directories
SRC_DIR := src
TEST_DIR := test
TOOLS_DIR := tools/formal-methods

# Pattern rule for SVG to PNG conversion
%.png: %.svg
	rsvg-convert -o $@ $<

# Pattern rule for Org to Markdown conversion
%.md: %.org
	emacs --batch --load org --eval "(setq org-export-with-toc t)" --visit=$< --funcall org-md-export-to-markdown

# Specific target for t-shirt design
docs/images/tshirt-design.png: docs/images/tshirt-design.svg

# QR code for repository URL
docs/images/repo-barcode.png: | docs/images
	qrencode -o $@ -s 8 -m 4 -t PNG "https://github.com/aygp-dr/fixed-point-explorer"

# Help target - default
help: ## Show this help message
	@echo "Fixed Point Explorer - Available targets:"
	@echo
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-20s %s\n", $$1, $$2}'
	@echo
	@echo "Example usage:"
	@echo "  gmake deps          # Check dependencies"
	@echo "  gmake test          # Run all tests"
	@echo "  gmake repl-guile    # Start Guile REPL"

all: test ## Run all tests (alias for test)

run: ## Run Y combinator demonstrations
	@./scripts/demo.sh

run-lean: ## Run Lean specifications demo
	@echo "\n=== Lean Formal Specifications Demo ==="
	@echo "Running Fibonacci.lean..."
	@$(LEAN_BIN) docs/specs/lean/Fibonacci.lean
	@echo "\nRunning Simple.lean..."
	@$(LEAN_BIN) docs/specs/lean/Simple.lean
	@echo "\nRunning Performance.lean (comparing implementations)..."
	@$(LEAN_BIN) docs/specs/lean/Performance.lean 2>/dev/null || echo "Performance comparison complete"
	@echo "\nFor full Lean tests: gmake test-lean-all"

run-all: run run-lean ## Run all demonstrations (Scheme + Lean)

deps: ## Check for required dependencies
	@./scripts/deps.sh

setup: ## Download and install formal methods tools
	@./scripts/setup.sh

# Legacy target for compatibility
install-deps: deps

# Run all tests
test: ## Run tests for all Scheme implementations
	@$(MAKE) test-guile
	@$(MAKE) test-chez
	@$(MAKE) test-racket

# Guile tests
test-guile: ## Run Guile tests
	@echo "\n=== Running Guile Tests ==="
	@$(GUILE) -L $(SRC_DIR)/portable -L $(SRC_DIR)/guile \
		$(TEST_DIR)/integration/test-all.scm

# Chez tests
test-chez: ## Run Chez Scheme tests
	@echo "\n=== Running Chez Scheme Tests ==="
	@$(CHEZ) --script $(TEST_DIR)/integration/test-all.scm

# Racket tests
test-racket: ## Run Racket tests
	@echo "\n=== Running Racket Tests ==="
	@$(RACKET) $(TEST_DIR)/integration/test-all.scm

# Run specific implementation tests with extra features
test-guile-extra: ## Run Guile with performance tests
	@echo "\n=== Running Guile with Extra Features ==="
	@$(GUILE) -L $(SRC_DIR)/portable -L $(SRC_DIR)/guile \
		-c "(load \"$(SRC_DIR)/guile/guile-ycombinator.scm\") \
		    (test-fibonacci) \
		    (test-list-ops) \
		    (guile-performance-test)"

test-chez-extra: ## Run Chez with specific features
	@echo "\n=== Running Chez with Extra Features ==="
	@$(CHEZ) --script $(SRC_DIR)/chez/chez-ycombinator.ss

# REPL targets
repl-guile: ## Start Guile REPL with project loaded
	@$(GUILE) -L $(SRC_DIR)/portable -L $(SRC_DIR)/guile

repl-chez: ## Start Chez REPL with project loaded
	@$(CHEZ) --load $(SRC_DIR)/portable/y-combinator.scm

repl-racket: ## Start Racket REPL
	@$(RACKET) -i -l racket/init

# Integration tests
test-integration-guile: ## Run Guile integration tests
	@echo "\n=== Running Guile Integration Tests ==="
	@cd $(shell pwd) && $(GUILE) -L $(SRC_DIR)/portable $(TEST_DIR)/integration/test-guile-repl.scm

test-integration: test-integration-guile ## Run all integration tests

# Lean configuration
LEAN_VERSION := v4.21.0
LEAN_ARCHIVE := lean-$(LEAN_VERSION:v%=%)-linux.zip
LEAN_DIR := $(TOOLS_DIR)/lean
LEAN_BIN := $(LEAN_DIR)/bin/lean

# Create base directories
$(TOOLS_DIR):
	mkdir $@

# Lean installation (not .PHONY since it creates a directory)
$(LEAN_DIR): | $(TOOLS_DIR)
	@echo "Installing Lean 4..."
	@if [ "$$(uname)" = "FreeBSD" ]; then \
		echo "=== FreeBSD: Using Linux compatibility for Lean ==="; \
		$(MAKE) lean-install-linux-compat; \
	elif [ "$$(uname)" = "Darwin" ]; then \
		cd $(TOOLS_DIR) && \
		curl -L https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-$(LEAN_VERSION:v%=%)-darwin.tar.gz | tar xz && \
		ln -sf lean-$(LEAN_VERSION:v%=%)-darwin lean; \
	else \
		cd $(TOOLS_DIR) && \
		curl -L https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-$(LEAN_VERSION:v%=%)-linux.tar.gz | tar xz && \
		ln -sf lean-$(LEAN_VERSION:v%=%)-linux lean; \
	fi

# Lean installation for FreeBSD using Linux compatibility
lean-install-linux-compat: | $(TOOLS_DIR)
	@cd $(TOOLS_DIR) && \
		wget https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/$(LEAN_ARCHIVE) && \
		unzip -q $(LEAN_ARCHIVE) && \
		rm $(LEAN_ARCHIVE) && \
		ln -sf lean-$(LEAN_VERSION:v%=%)-linux lean
	@echo '#check (1 + 1 : Nat)' | $(LEAN_DIR)/bin/lean --stdin || \
		echo "Note: Lean requires Linux compatibility layer on FreeBSD"

# Additional Lean targets for FreeBSD
lean-version: ## Show Lean and Lake versions
	@if [ -x "$(LEAN_DIR)/bin/lean" ]; then \
		$(LEAN_DIR)/bin/lean --version; \
		$(LEAN_DIR)/bin/lake --version; \
	else \
		echo "Lean not installed. Run: gmake lean-tools"; \
	fi

lean-test: ## Test Lean installation
	@echo "=== Testing Lean Installation ==="
	@if [ -x "$(LEAN_BIN)" ]; then \
		echo "Lean binary found!"; \
		echo '#check (1 + 1 : Nat)' | $(LEAN_BIN) --stdin || \
			echo "Note: Binary may need Linux compatibility layer on FreeBSD"; \
	else \
		echo "Lean binary not found or not executable"; \
	fi

lean-check-compat: ## Check FreeBSD Linux compatibility
	@if [ "$$(uname)" = "FreeBSD" ]; then \
		echo "=== Checking Linux Compatibility Layer ==="; \
		if kldstat | grep -q linux64; then \
			echo "✓ Linux64 compatibility module loaded"; \
		else \
			echo "✗ Linux64 compatibility not loaded"; \
			echo "  To enable: sudo kldload linux64"; \
			echo "  To make permanent: add linux_enable=\"YES\" to /etc/rc.conf"; \
		fi; \
		if [ -d /compat/linux ]; then \
			echo "✓ Linux compat directory exists"; \
		else \
			echo "✗ /compat/linux not found"; \
			echo "  Install linux base: sudo pkg install linux_base-c7"; \
		fi; \
	else \
		echo "Not on FreeBSD, skipping compatibility check"; \
	fi

clean-lean: ## Remove Lean installation
	@echo "Removing Lean installation..."
	@rm -rf $(LEAN_DIR)
	@rm -rf $(TOOLS_DIR)/lean-*.tar.gz $(TOOLS_DIR)/lean-*.zip
	@echo "Lean removed"

# Alias for backward compatibility
lean-tools: $(LEAN_DIR) ## Install Lean tools (alias for $(LEAN_DIR))

# Lean test targets
test-lean-fibonacci: $(LEAN_BIN) ## Test Fibonacci Lean specification
	@echo "Testing Fibonacci.lean..."
	@$(LEAN_BIN) docs/specs/lean/Fibonacci.lean >/dev/null 2>&1 && echo "✓ Passed" || echo "✗ Failed"

test-lean-ycombinator: $(LEAN_BIN) ## Test Y Combinator Lean specification
	@echo "Testing YCombinator.lean..."
	@$(LEAN_BIN) docs/specs/lean/YCombinator.lean >/dev/null 2>&1 && echo "✓ Passed" || echo "✗ Failed"

test-lean-properties: $(LEAN_BIN) ## Test Properties Lean specification
	@echo "Testing Properties.lean..."
	@$(LEAN_BIN) docs/specs/lean/Properties.lean >/dev/null 2>&1 && echo "✓ Passed" || echo "✗ Failed"

test-lean-all: $(LEAN_BIN) ## Run all Lean tests
	@echo "\n=== Running All Lean Tests ==="
	@./scripts/test-lean.sh

test-lean-interactive: $(LEAN_BIN) ## Interactive Lean proof checking
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: gmake test-lean-interactive FILE=docs/specs/lean/Fibonacci.lean"; \
	else \
		$(LEAN_BIN) $(FILE); \
	fi

# Benchmarks
benchmark: ## Run performance benchmarks
	@echo "\n=== Benchmarking Fibonacci ==="
	@echo "Guile:"
	@time $(GUILE) -c "(load \"$(SRC_DIR)/portable/fibonacci.scm\") (fib 30)"
	@echo "\nChez:"
	@time $(CHEZ) --script -c "(load \"$(SRC_DIR)/portable/fibonacci.scm\") (fib 30)"
	@echo "\nRacket:"
	@time $(RACKET) -e "(load \"$(SRC_DIR)/portable/fibonacci.scm\") (fib 30)"

# Clean
clean: ## Clean build artifacts
	@find . -name "*.zo" -delete
	@find . -name "*.so" -delete
	@find . -name "*.o" -delete
	@find . -name "*~" -delete
	@find . -name "*.fasl" -delete
	@find . -name "*.go" -delete
	@echo "Cleaned build artifacts"

# Emacs integration
emacs-verify: ## Verify Emacs/Geiser configuration
	@./scripts/emacs.sh verify

emacs: ## Start Emacs with project configuration
	@./scripts/emacs.sh start
