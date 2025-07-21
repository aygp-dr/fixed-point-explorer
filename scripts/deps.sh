#!/bin/sh
# deps.sh - Check for required dependencies

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track missing dependencies
MISSING_DEPS=""

check_command() {
    local cmd=$1
    local name=${2:-$1}
    local install_hint=$3
    
    if command -v "$cmd" >/dev/null 2>&1; then
        printf "${GREEN}✓${NC} %s found: %s\n" "$name" "$(command -v "$cmd")"
        return 0
    else
        printf "${RED}✗${NC} %s not found\n" "$name"
        if [ -n "$install_hint" ]; then
            printf "  ${YELLOW}→ %s${NC}\n" "$install_hint"
        fi
        MISSING_DEPS="${MISSING_DEPS}${name} "
        return 1
    fi
}

echo "Checking required dependencies..."
echo

# Check for Scheme implementations
echo "Scheme implementations:"
check_command guile3 "GNU Guile 3" "Install: pkg install guile3 (FreeBSD) | apt install guile-3.0 (Debian/Ubuntu)"
check_command chez-scheme "Chez Scheme" "Install: pkg install chez-scheme (FreeBSD) | apt install chezscheme (Debian/Ubuntu)"
check_command racket "Racket" "Install: pkg install racket (FreeBSD) | apt install racket (Debian/Ubuntu)"

echo
echo "Build tools:"
check_command gmake "GNU Make" "Install: pkg install gmake (FreeBSD) | apt install make (Linux)"
check_command git "Git" "Install: pkg install git"

echo
echo "Optional tools:"
check_command emacs "Emacs (for Geiser)" "Install: pkg install emacs"
check_command rg "ripgrep" "Install: pkg install ripgrep"

# Check if we're missing critical dependencies
if [ -n "$MISSING_DEPS" ]; then
    echo
    echo "${RED}Missing dependencies:${NC} $MISSING_DEPS"
    echo "Please install missing dependencies before continuing."
    exit 1
else
    echo
    echo "${GREEN}All required dependencies are installed!${NC}"
    exit 0
fi