# Observation: 2025-07-21 - Makefile Best Practices for File-Based Targets

## Summary
Implementing proper file-based targets for Lean installation following Make best practices.

## Best Practices Makefile Implementation

```makefile
# === Configuration ===
LEAN_VERSION := 4.21.0
LEAN_RELEASE := v$(LEAN_VERSION)
TOOLS_DIR := tools/formal-methods
LEAN_ZIP := $(TOOLS_DIR)/lean-$(LEAN_VERSION)-linux.zip
LEAN_DIR := $(TOOLS_DIR)/lean-$(LEAN_VERSION)-linux
LEAN_BIN := $(LEAN_DIR)/bin/lean
LEAN_LINK := $(TOOLS_DIR)/lean4

# === Directory Creation (automatic with | order-only prerequisite) ===
$(TOOLS_DIR):
	@install -d $@

# === Download Target (file-based, idempotent) ===
$(LEAN_ZIP): | $(TOOLS_DIR)
	@echo "Downloading Lean $(LEAN_VERSION)..."
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/$(LEAN_RELEASE)/lean-$(LEAN_VERSION)-linux.zip

# === Extract Target (depends on zip file) ===
$(LEAN_BIN): $(LEAN_ZIP)
	@echo "Extracting Lean $(LEAN_VERSION)..."
	@cd $(TOOLS_DIR) && unzip -q $(notdir $<)
	@touch $@  # Update timestamp to prevent re-extraction

# === Symlink Target (for version-agnostic access) ===
$(LEAN_LINK): $(LEAN_BIN)
	@echo "Creating symlink to Lean $(LEAN_VERSION)..."
	@ln -sf $(notdir $(LEAN_DIR)) $@

# === Test Target (verifies installation) ===
$(TOOLS_DIR)/.lean-tested: $(LEAN_BIN)
	@echo "Testing Lean installation..."
	@echo '#check (1 + 1 : Nat)' | $< --stdin >/dev/null
	@$< --version
	@$(dir $<)/lake --version
	@touch $@

# === Public Targets (aliases) ===
.PHONY: lean-install lean-version lean-clean

lean-install: $(TOOLS_DIR)/.lean-tested $(LEAN_LINK)
	@echo "Lean $(LEAN_VERSION) ready at: $(LEAN_LINK)"

lean-version: $(LEAN_BIN)
	@$< --version 2>/dev/null || echo "Lean not installed"
	@$(dir $<)/lake --version 2>/dev/null || true

lean-clean:
	@rm -f $(LEAN_ZIP) $(TOOLS_DIR)/.lean-tested
	@rm -rf $(LEAN_DIR)
	@rm -f $(LEAN_LINK)

# === Pattern Rules for Future Versions ===
# Allow: gmake tools/formal-methods/lean-4.22.0-linux.zip
$(TOOLS_DIR)/lean-%-linux.zip: | $(TOOLS_DIR)
	@echo "Downloading Lean $*..."
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/v$*/lean-$*-linux.zip

# === Automatic Directory Creation for Any Path ===
# This allows: gmake any/path/to/file
# and creates directories as needed
%/:
	@install -d $@

# Precious files (don't delete even if Make is interrupted)
.PRECIOUS: $(LEAN_ZIP) $(TOOLS_DIR)/lean-%-linux.zip
```

## Key Best Practices Illustrated

### 1. **File-Based Targets**
- Targets are actual files (`$(LEAN_ZIP)`, `$(LEAN_BIN)`)
- Make checks timestamps automatically
- Idempotent: running multiple times is safe

### 2. **Order-Only Prerequisites**
- `| $(TOOLS_DIR)` creates directory if needed
- Won't trigger rebuild if directory timestamp changes

### 3. **Automatic Variables**
- `$@` = target being built
- `$<` = first prerequisite
- `$(dir $<)` = directory of first prerequisite
- `$(notdir $<)` = filename without directory

### 4. **Pattern Rules**
- `%-linux.zip` matches any version
- Enables: `gmake tools/formal-methods/lean-4.22.0-linux.zip`

### 5. **Precious Targets**
- `.PRECIOUS` prevents deletion of downloaded files
- Important for large downloads

### 6. **Sentinel Files**
- `.lean-tested` tracks successful test
- Prevents re-testing on every make

## Usage Examples

```bash
# Install default version
gmake lean-install

# Download specific version
gmake tools/formal-methods/lean-4.22.0-linux.zip

# Check what would be done
gmake -n lean-install

# Force reinstall
gmake lean-clean lean-install

# Just download, don't extract
gmake tools/formal-methods/lean-4.21.0-linux.zip
```

## Benefits

1. **Efficiency**: Only downloads/extracts when needed
2. **Parallelism**: Can use `make -j4` safely
3. **Debuggable**: Each step creates a file
4. **Resumable**: Interrupted downloads can be resumed
5. **Versionable**: Easy to test different Lean versions