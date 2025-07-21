# Observation: 2025-07-21 - Lean 4 Setup Instructions for Builder

## Summary
Instructions for Builder to implement Lean 4 installation in the Makefile based on successful Linux compatibility experiment.

## Implementation Details

### Tool Versions
- **Lean**: 4.21.0
- **Lake**: 5.0.0-6741444 (bundled with Lean)
- **Platform**: Linux binaries on FreeBSD via compatibility layer

### Makefile Target Suggestion

```makefile
LEAN_VERSION := v4.21.0
LEAN_DIR := tools/formal-methods/lean4

lean-install-linux-compat:
	@echo "Installing Lean $(LEAN_VERSION) via Linux compatibility..."
	@mkdir -p tools/formal-methods
	@cd tools/formal-methods && \
		curl -L -o lean-linux.zip \
		https://github.com/leanprover/lean4/releases/download/$(LEAN_VERSION)/lean-4.21.0-linux.zip && \
		unzip -q lean-linux.zip && \
		rm lean-linux.zip && \
		ln -sf lean-4.21.0-linux lean4
	@echo "Testing installation..."
	@echo '#check (1 + 1 : Nat)' | $(LEAN_DIR)/bin/lean --stdin
	@echo "Lean $(LEAN_VERSION) installed successfully!"

lean-version:
	@if [ -x "$(LEAN_DIR)/bin/lean" ]; then \
		$(LEAN_DIR)/bin/lean --version; \
		$(LEAN_DIR)/bin/lake --version; \
	else \
		echo "Lean not installed. Run: gmake lean-install-linux-compat"; \
	fi
```

### Badge Information

For README badges, use:
- **Lean Version**: 4.21.0
- **Lake Version**: 5.0.0
- **Method**: Linux Compat

Example badge URLs:
```markdown
![Lean](https://img.shields.io/badge/Lean-4.21.0-blue)
![Lake](https://img.shields.io/badge/Lake-5.0.0-green)
![Platform](https://img.shields.io/badge/Platform-FreeBSD_Linux_Compat-orange)
```

### Notes for Builder
1. This method requires Linux compatibility layer enabled on FreeBSD
2. Binary location: `tools/formal-methods/lean4/bin/`
3. The symlink `lean4` points to the versioned directory for easier updates
4. Consider adding `.gitignore` entries for the tools directory

## References
- GitHub Issue: #1
- Experiment: `experiments/2025-01-21-lean-linux-compat/`