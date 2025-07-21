# Observation: 2025-07-21 - Org-Babel Detangle Limitation

## Issue
org-babel-detangle is not propagating changes from tangled elisp files back to the org file.

## Test Case
1. ✅ Modified factorial.el with "CONTINUE" magic string and prime calculation
2. ✅ Ran `gmake detangle` - reported "Detangled 0 code blocks" 
3. ✅ Tried `org-babel-detangle` in tmux Emacs session
4. ❌ Changes not reflected in y-combinator.org

## Root Cause
Org-babel detangling requires:
1. The elisp file to have proper org-babel markers (which it does)
2. The org file to be open in Emacs when detangling
3. Possibly specific configuration for detangling

## Workaround
The standard workflow is:
- Edit org file → tangle to elisp (one-way)
- For reverse flow: manually copy changes back to org blocks

## Recommendation for Builder
Document this limitation and suggest editing the org file directly for bidirectional literate programming, or implement a custom detangle solution.