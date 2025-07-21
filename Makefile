# Makefile for fixed-point-explorer
# Default target is help
.DEFAULT_GOAL := help

# Use gmake explicitly
MAKE := gmake

.PHONY: help all test test-guile test-chez test-racket clean deps setup lean-tools
.PHONY: repl-guile repl-chez repl-racket benchmark test-guile-extra test-chez-extra
.PHONY: lean-version install-deps

# Scheme implementations
GUILE := guile3
CHEZ := chez-scheme
RACKET := racket

# Directories
SRC_DIR := src
TEST_DIR := test
TOOLS_DIR := tools/formal-methods

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

# Lean configuration
LEAN_VERSION := v4.21.0
LEAN_DIR := $(TOOLS_DIR)/lean

# Lean tools (matching your pattern)
$(TOOLS_DIR)/lean:
	@echo "Installing Lean 4..."
	@mkdir -p $(TOOLS_DIR)
	@if [ "$$(uname)" = "FreeBSD" ]; then \
		echo "=== FreeBSD: Using Linux compatibility for Lean ==="; \
		$(MAKE) lean-install-linux-compat; \
	elif [ "$$(uname)" = "Darwin" ]; then \
		curl -L https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-4.21.0-darwin.tar.gz | tar xz -C $(TOOLS_DIR); \
		ln -sf $(TOOLS_DIR)/lean-4.21.0-darwin $(LEAN_DIR); \
	else \
		curl -L https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-4.21.0-linux.tar.gz | tar xz -C $(TOOLS_DIR); \
		ln -sf $(TOOLS_DIR)/lean-4.21.0-linux $(LEAN_DIR); \
	fi

# Lean installation for FreeBSD using Linux compatibility
lean-install-linux-compat: ## Install Lean 4 using Linux compatibility
	@mkdir -p $(TOOLS_DIR)
	@cd $(TOOLS_DIR) && \
		wget https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-4.21.0-linux.zip && \
		unzip -q lean-4.21.0-linux.zip && \
		rm lean-4.21.0-linux.zip && \
		ln -sf lean-4.21.0-linux lean
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
	@if [ -x "$(LEAN_DIR)/bin/lean" ]; then \
		echo "Lean binary found!"; \
		echo '#check (1 + 1 : Nat)' | $(LEAN_DIR)/bin/lean --stdin || \
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

lean-tools: $(TOOLS_DIR)/lean

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
