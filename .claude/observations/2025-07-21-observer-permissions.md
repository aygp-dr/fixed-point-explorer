# Observation: 2025-07-21 - Observer Permissions Clarification

## Summary
Observer role permissions clarified - experiments/ directory is a sandbox for Observer work.

## Permissions Model

### Observer CAN:
- **Read** all source files
- **Write** to:
  - `.claude/observations/` - Analysis and insights
  - `experiments/` - Testing and exploration (Builder ignores)
  - GitHub issues with 'observer'/'meta' labels
- **Modify** within experiments/ for testing purposes

### Observer CANNOT:
- Modify source code outside experiments/
- Make commits (Builder's responsibility)
- Change project configuration files
- Push to repository

## Working Model
```
fixed-point-explorer/
├── src/              # READ-ONLY for Observer
├── docs/             # READ-ONLY for Observer
├── .claude/
│   └── observations/ # Observer writes here
└── experiments/      # Observer sandbox
    └── */            # Can create/modify anything here
```

## Benefits
1. **Safe experimentation** - Test ideas without affecting main codebase
2. **Builder isolation** - Builder knows to ignore experiments/
3. **Clear boundaries** - No accidental source modifications
4. **Traceable work** - All Observer experiments documented

## Current Experiments
- `2025-01-21-lean-linux-compat/` - Successfully tested Lean compatibility
- Future: Can test Y combinator variations, performance experiments, etc.

This model allows Observer to be more helpful while maintaining clean separation of concerns.