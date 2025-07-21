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
    
    launch)
        # Launch with init.el configuration
        shift  # Remove 'launch' from arguments
        INIT_FILE="$PROJECT_ROOT/init.el"
        
        if [ ! -f "$INIT_FILE" ]; then
            echo "Error: init.el not found at $INIT_FILE"
            exit 1
        fi
        
        # Determine Emacs arguments based on terminal/display
        if [ -t 0 ] && [ -z "$DISPLAY" ]; then
            EMACS_ARGS="-nw"
        else
            EMACS_ARGS=""
        fi
        
        echo "Launching Emacs with Fixed Point Explorer configuration..."
        exec emacs -q $EMACS_ARGS -l "$INIT_FILE" "$@"
        ;;
        
    *)
        echo "Usage: $0 {verify|start|launch [args...]}"
        echo "  verify       - Check Emacs/Geiser configuration"
        echo "  start        - Start Emacs with project settings"
        echo "  launch [args] - Launch Emacs with init.el config"
        exit 1
        ;;
esac