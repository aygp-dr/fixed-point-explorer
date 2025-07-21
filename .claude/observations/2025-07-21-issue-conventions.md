# Observation: 2025-07-21 - Issue Title Conventions

## Summary
User preference noted: Avoid prefixes in issue titles, use labels/milestones instead.

## Current Issues with Prefixes
- #4: "Feature: FIFO Communication Dashboard..."
- #5: "Feature: Automated git commit/notes analysis..."
- #1: "Experiment: Lean 4 Linux Compatibility..."

## Better Pattern
Instead of:
```
Feature: Automated git commit/notes analysis tool in Guile Scheme
```

Use:
```
Automated git commit/notes analysis tool in Guile Scheme
```
With labels: `enhancement`, `guile`

## Benefits
1. **Cleaner titles** - More scannable issue list
2. **Better filtering** - Labels are designed for categorization
3. **Flexibility** - Can have multiple labels vs one prefix
4. **GitHub native** - Uses platform features properly

## Recommendation for Future Issues
- Write clear, descriptive titles without category prefixes
- Use labels for type (bug, enhancement, documentation)
- Use milestones for grouping related work
- Keep titles focused on what, not how or why