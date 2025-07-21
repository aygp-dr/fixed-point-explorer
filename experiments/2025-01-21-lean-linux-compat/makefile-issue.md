# Makefile Best Practices: File-based targets for Lean installation

## Issue: Improve Makefile maintainability with proper file-based targets

### Problem
Current Makefile approach mixes hardcoded values with variables, making maintenance difficult:
```makefile
# Bad - mixed variables and literals
curl ... $(LEAN_VERSION)/lean-4.21.0-linux.zip  # Variable and hardcoded!
```

### Proposed Solution
Implement proper file-based targets following GNU Make best practices.

### Best Practices Research

#### 1. **File-based targets over phony targets** (GNU Make Manual §4.2)
- Targets should be actual files that get created
- Make tracks timestamps automatically
- Enables proper dependency management

#### 2. **Automatic variables** (GNU Make Manual §10.5.3)
- `$@` = The target being built
- `$<` = The first prerequisite
- `$(dir ...)` = Directory portion
- `$(notdir ...)` = File portion without directory

#### 3. **Order-only prerequisites** (GNU Make Manual §4.3)
- Use `|` for directories that need to exist but shouldn't trigger rebuilds
- Example: `$(TARGET): prereq | $(DIR)`

#### 4. **Pattern rules** (GNU Make Manual §10.5)
- Enable generic targets like `%.zip` for any version
- DRY principle - Don't Repeat Yourself

### Recommended Implementation

```makefile
# === Configuration (single source of truth) ===
LEAN_VERSION := 4.21.0
LEAN_RELEASE := v$(LEAN_VERSION)
LEAN_ARCHIVE := lean-$(LEAN_VERSION)-linux
TOOLS_DIR := tools/formal-methods
LEAN_ZIP := $(TOOLS_DIR)/$(LEAN_ARCHIVE).zip
LEAN_BIN := $(TOOLS_DIR)/$(LEAN_ARCHIVE)/bin/lean
LEAN_LINK := $(TOOLS_DIR)/lean4

# === Directory creation ===
$(TOOLS_DIR):
	@install -d $@

# === Download (creates actual file) ===
$(LEAN_ZIP): | $(TOOLS_DIR)
	@echo "Downloading Lean $(LEAN_VERSION)..."
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/$(LEAN_RELEASE)/$(notdir $@)

# === Extract (depends on zip) ===
$(LEAN_BIN): $(LEAN_ZIP)
	@echo "Extracting $(notdir $<)..."
	@cd $(dir $<) && unzip -q $(notdir $<)
	@touch $@  # Update timestamp

# === Symlink (version-agnostic access) ===
$(LEAN_LINK): $(LEAN_BIN)
	@ln -sf $(LEAN_ARCHIVE) $@

# === Test installation ===
$(TOOLS_DIR)/.lean-$(LEAN_VERSION)-tested: $(LEAN_BIN)
	@echo '#check (1 + 1 : Nat)' | $< --stdin >/dev/null
	@$< --version
	@touch $@

# === Public targets ===
.PHONY: lean-install lean-clean
lean-install: $(TOOLS_DIR)/.lean-$(LEAN_VERSION)-tested $(LEAN_LINK)

lean-clean:
	@rm -rf $(LEAN_ZIP) $(LEAN_DIR) $(LEAN_LINK) $(TOOLS_DIR)/.lean-*-tested

# === Pattern rule for any version ===
$(TOOLS_DIR)/lean-%-linux.zip: | $(TOOLS_DIR)
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/v$*/$(notdir $@)

# === Precious files (don't delete on interrupt) ===
.PRECIOUS: $(LEAN_ZIP) $(TOOLS_DIR)/lean-%-linux.zip
```

### Benefits
1. **Single source of truth**: Change `LEAN_VERSION` once, everything updates
2. **Idempotent**: Run multiple times safely
3. **Resumable**: Interrupted downloads preserved
4. **Debuggable**: Each step creates a traceable file
5. **Parallel-safe**: Works with `make -j`

### Usage Examples
```bash
# Install default version
gmake lean-install

# Download specific version  
gmake tools/formal-methods/lean-4.22.0-linux.zip

# Check installation
gmake lean-version

# Clean and reinstall
gmake lean-clean lean-install
```

### References
- [GNU Make Manual](https://www.gnu.org/software/make/manual/)
- [Make Tutorial - File Targets](https://makefiletutorial.com/#file-targets)
- [Recursive Make Considered Harmful](https://aegis.sourceforge.net/auug97.pdf)
- [Auto-Dependency Generation](https://make.mad-scientist.net/papers/advanced-auto-dependency-generation/)

### Related
- Experiment results: #1
- Observer notes: `.claude/observations/2025-07-21-make-best-practices.md`