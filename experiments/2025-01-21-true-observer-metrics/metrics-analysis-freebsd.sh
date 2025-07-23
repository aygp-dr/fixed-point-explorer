#!/bin/bash
# Project Metrics Analysis Tools - FreeBSD Compatible
# Enhanced by Observer based on TRUE-OBSERVER's original

echo "=== PROJECT METRICS ANALYSIS ==="
echo "Date: $(date)"
echo "Project: $(basename $(pwd))"
echo "System: $(uname -s)"
echo

# Core functional code (actual implementations)
echo "CORE FUNCTIONAL CODE:"
echo "===================="
find ./src -name "*.scm" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Core Scheme lines:", $1}'
find ./docs/specs/lean -name "*.lean" -not -path "*/.lake/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Project Lean lines:", $1}' || echo "Project Lean lines: 0"
find ./experiments -name "*.el" -path "*/2025-01-21-elisp-y-combinator/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Elisp Y-combinator lines:", $1}' || echo "Elisp Y-combinator lines: 0"

# Test infrastructure (separate from core)
echo
echo "TEST INFRASTRUCTURE:"
echo "==================="
find ./test -name "*.scm" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Test framework lines:", $1}' || echo "Test framework lines: 0"

# Documentation and meta-files
echo
echo "DOCUMENTATION:"
echo "=============="
# FreeBSD-compatible date for 24 hours ago
YESTERDAY=$(date -v-1d +%Y-%m-%d)
find . -name "*.md" -newer /tmp -not -path "./.git/*" | wc -l | awk '{print "Total markdown files:", $1}'
find . -name "*.org" -not -path "./.git/*" | wc -l | awk '{print "Total org files:", $1}'
find . -name "*.md" -not -path "./.git/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Documentation lines (MD):", $1}'
find . -name "*.org" -not -path "./.git/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print "Documentation lines (ORG):", $1}'

# GitHub activity
echo
echo "GITHUB ACTIVITY:"
echo "================"
gh issue list --limit 100 --json number,title,createdAt,labels | jq -r '.[] | select(.createdAt | startswith("'$(date +%Y-%m-%d)'")) | .number' | wc -l | awk '{print "Issues created today:", $1}'
gh issue list --limit 100 --json labels | jq -r '.[].labels[].name' | sort | uniq -c | sort -nr | head -10

# Git activity
echo
echo "GIT ACTIVITY:"
echo "============="
git log --oneline --since="$(date +%Y-%m-%d) 00:00" | wc -l | awk '{print "Commits today:", $1}'
git log --oneline --since="$(date -v-7d +%Y-%m-%d) 00:00" | wc -l | awk '{print "Commits last 7 days:", $1}'

# Observer patterns
echo
echo "OBSERVER PATTERNS:"
echo "=================="
find ./.claude/observations -name "*.md" 2>/dev/null | wc -l | awk '{print "Observer documents:", $1}'
find ./experiments -type d -name "2025-*" | wc -l | awk '{print "Experiment folders:", $1}'

echo
echo "=== DOCUMENTATION TO CODE RATIO ==="
TOTAL_CODE=$(find ./src -name "*.scm" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}')
TOTAL_DOCS=$(find . -name "*.md" -not -path "./.git/*" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}')
echo "Core code lines: $TOTAL_CODE"
echo "Documentation lines: $TOTAL_DOCS"
if [ "$TOTAL_CODE" -gt 0 ]; then
    RATIO=$(echo "scale=2; $TOTAL_DOCS / $TOTAL_CODE" | bc)
    echo "Doc:Code ratio: ${RATIO}:1"
fi

echo
echo "=== LANGUAGE DISTRIBUTION ==="
echo "Scheme files:" $(find . -name "*.scm" -not -path "./.git/*" | wc -l)
echo "Lean files:" $(find . -name "*.lean" -not -path "*/.lake/*" -not -path "./.git/*" | wc -l)
echo "Elisp files:" $(find . -name "*.el" -not -path "./.git/*" | wc -l)
echo "Org files:" $(find . -name "*.org" -not -path "./.git/*" | wc -l)

echo
echo "=== EXCLUSIONS ==="
echo "Excluded from analysis:"
echo "- .lake/ directory (Lean dependencies)"
echo "- .git/ directory"
echo "- Generated/temporary files"
echo "- External dependencies"