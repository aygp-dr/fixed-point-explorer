# Observation: 2025-07-21 - Makefile.lean Issues

## Summary
The existing Makefile.lean has several issues discovered during manual testing.

## Issues Found

### 1. Wrong URL Format
The Makefile uses `.tar.gz` but Lean 4.5.0+ uses `.tar.zst` format:
```makefile
# Wrong
https://github.com/leanprover/lean4/releases/download/v4.5.0/lean-4.5.0-linux.tar.gz

# Correct for newer versions
https://github.com/leanprover/lean4/releases/download/v4.21.0/lean-4.21.0-linux_x86_64.tar.zst
```

### 2. Architecture Detection Missing
Need to detect architecture (x86_64 vs aarch64):
```makefile
ARCH := $(shell uname -m)
ifeq ($(ARCH),x86_64)
    LEAN_ARCH := x86_64
else ifeq ($(ARCH),amd64)
    LEAN_ARCH := x86_64
else ifeq ($(ARCH),aarch64)
    LEAN_ARCH := aarch64
endif
```

### 3. Compression Format Change
Need `zstd` for newer releases:
```makefile
# Check for zstd
check-zstd:
	@command -v zstd >/dev/null 2>&1 || \
		(echo "Error: zstd required. Run: sudo pkg install zstd" && exit 1)

# Extract with zstd
tar --zstd -xf lean-$(LEAN_VERSION)-linux_$(LEAN_ARCH).tar.zst
```

### 4. Version Mismatch
Makefile hardcodes 4.5.0 but experiment uses 4.21.0

## Recommended Fix

```makefile
# Configuration
LEAN_VERSION := 4.21.0
ARCH := $(shell uname -m | sed 's/amd64/x86_64/')
LEAN_ARCHIVE := lean-$(LEAN_VERSION)-linux_$(ARCH)

# For 4.21.0 and newer (uses zst)
$(TOOLS_DIR)/$(LEAN_ARCHIVE).tar.zst: | check-zstd $(TOOLS_DIR)
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/v$(LEAN_VERSION)/$(notdir $@)

# Extract
$(TOOLS_DIR)/$(LEAN_ARCHIVE)/bin/lean: $(TOOLS_DIR)/$(LEAN_ARCHIVE).tar.zst
	@cd $(dir $<) && tar --zstd -xf $(notdir $<)
	@touch $@
```

## Notes
- User downloaded wrong architecture file (aarch64 instead of x86_64)
- The `.zip` format from experiment works without zstd requirement
- Consider supporting both .zip and .tar.zst formats