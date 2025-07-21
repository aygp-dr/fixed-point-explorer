#!/bin/sh
# emacs.sh - Emacs/Geiser integration utilities

# Get script directory and project root
SCRIPT_DIR=$(dirname "$0")
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

case "$1" in
    verify)
        echo "=== Verifying Emacs Configuration ==="
        
        # Check .dir-locals.el
        if [ -f "$PROJECT_ROOT/.dir-locals.el" ]; then
            printf "${GREEN}✓${NC} .dir-locals.el found\n"
        else
            printf "${RED}✗${NC} .dir-locals.el missing\n"
        fi
        
        # Check geiser config
        if [ -f "$PROJECT_ROOT/lib/geiser/geiser-config.el" ]; then
            printf "${GREEN}✓${NC} lib/geiser/geiser-config.el found\n"
        else
            printf "${RED}✗${NC} lib/geiser/geiser-config.el missing\n"
        fi
        
        # Check Emacs
        echo "\nChecking Emacs availability..."
        if command -v emacs >/dev/null 2>&1; then
            printf "${GREEN}✓${NC} Emacs found: $(emacs --version | head -1)\n"
            echo "\nTo load project in Emacs:"
            echo "  1. Start Emacs in project root"
            echo "  2. Open any .scm file"
            echo "  3. M-x geiser-mode"
            echo "  4. C-c C-z to start REPL"
        else
            printf "${RED}✗${NC} Emacs not found\n"
            echo "  Install: pkg install emacs (FreeBSD)"
        fi
        ;;
        
    start)
        if [ -f "$PROJECT_ROOT/.dir-locals.el" ]; then
            echo "Starting Emacs with project configuration..."
            cd "$PROJECT_ROOT" && \
            emacs --eval "(progn (find-file \"src/portable/y-combinator.scm\") (geiser-mode))" &
        else
            echo "Error: .dir-locals.el not found"
            exit 1
        fi
        ;;
        
    *)
        echo "Usage: $0 {verify|start}"
        echo "  verify - Check Emacs/Geiser configuration"
        echo "  start  - Start Emacs with project settings"
        exit 1
        ;;
esac