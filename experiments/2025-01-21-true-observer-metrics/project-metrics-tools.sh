#!/bin/bash
# Project Metrics Analysis Tools
# Tools used to generate accurate project summaries excluding dependencies

echo "=== PROJECT METRICS ANALYSIS ==="
echo "Date: $(date)"
echo "Project: $(basename $(pwd))"
echo

# Core functional code (actual implementations)
echo "CORE FUNCTIONAL CODE:"
echo "===================="
find ./src -name "*.scm" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Core Scheme lines:", $1}'
find ./docs/specs/lean -name "*.lean" -not -path "*/.lake/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Project Lean lines:", $1}' || echo "Project Lean lines: 0"

# Test infrastructure (separate from core)
echo
echo "TEST INFRASTRUCTURE:"
echo "==================="
find ./test -name "*.scm" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Test framework lines:", $1}' || echo "Test framework lines: 0"

# Documentation and meta-files
echo
echo "DOCUMENTATION:"
echo "=============="
find . -name "*.md" -newermt "$(date -d '1 day ago' '+%Y-%m-%d 00:00')" -not -path "./.git/*" | wc -l | awk '{print "Markdown files created today:", $1}'
find . -name "*.md" -newermt "$(date -d '1 day ago' '+%Y-%m-%d 00:00')" -not -path "./.git/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Documentation lines:", $1}'

# GitHub activity
echo
echo "GITHUB ACTIVITY:"
echo "================"
gh issue list --limit 20 --json number,title,createdAt | jq -r '.[] | select(.createdAt | startswith("'$(date +%Y-%m-%d)'")) | .number' | wc -l | awk '{print "Issues created today:", $1}'

# Git activity
echo
echo "GIT ACTIVITY:"
echo "============="
git log --oneline --since="$(date +%Y-%m-%d) 00:00" | wc -l | awk '{print "Commits today:", $1}'

echo
echo "=== EXCLUSIONS ==="
echo "Excluded from analysis:"
echo "- .lake/ directory (Lean dependencies)"
echo "- .git/ directory" 
echo "- Generated/temporary files"
echo "- External dependencies"

echo
echo "=== INDIVIDUAL SCHEME FILES ==="
find ./src -name "*.scm" -exec echo "File: {}" \; -exec wc -l {} \;

echo
echo "=== PROJECT LEAN SPECS ==="
find ./docs/specs/lean -name "*.lean" -not -path "*/.lake/*" -exec echo "File: {}" \; -exec wc -l {} \; 2>/dev/null || echo "No project Lean files"