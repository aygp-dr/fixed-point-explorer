#!/bin/sh
# test-lean.sh - Run all Lean tests from project root

# Get script directory and project root
SCRIPT_DIR=$(dirname "$0")
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Lean paths
LEAN_BIN="$PROJECT_ROOT/tools/formal-methods/lean/bin/lean"
LAKE_BIN="$PROJECT_ROOT/tools/formal-methods/lean/bin/lake"
LEAN_SPECS_DIR="$PROJECT_ROOT/docs/specs/lean"

# Check if Lean is installed
if [ ! -x "$LEAN_BIN" ]; then
    echo "${RED}Error:${NC} Lean not found at $LEAN_BIN"
    echo "Run 'gmake lean-tools' to install Lean"
    exit 1
fi

echo "=== Running Lean Tests from Project Root ==="
echo "Lean version: $($LEAN_BIN --version | head -1)"
echo ""

# Track results
TOTAL=0
PASSED=0
FAILED=0

# Test each Lean file
for file in "$LEAN_SPECS_DIR"/*.lean; do
    if [ -f "$file" ] && [ "$(basename "$file")" != "lakefile.lean" ]; then
        TOTAL=$((TOTAL + 1))
        filename=$(basename "$file")
        printf "Testing %-30s " "$filename..."
        
        if $LEAN_BIN "$file" >/dev/null 2>&1; then
            printf "${GREEN}✓ PASSED${NC}\n"
            PASSED=$((PASSED + 1))
        else
            printf "${RED}✗ FAILED${NC}\n"
            FAILED=$((FAILED + 1))
            # Show error details
            echo "  Error details:"
            $LEAN_BIN "$file" 2>&1 | head -10 | sed 's/^/    /'
        fi
    fi
done

echo ""
echo "=== Summary ==="
echo "Total:  $TOTAL"
echo "Passed: ${GREEN}$PASSED${NC}"
echo "Failed: ${RED}$FAILED${NC}"

# Exit with non-zero if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi

echo ""
echo "${GREEN}All Lean tests passed!${NC}"