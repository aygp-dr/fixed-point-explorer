#!/bin/sh
# setup.sh - Set up formal methods tools

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TOOLS_DIR="tools/formal-methods"
LEAN_VERSION="v4.5.0"

echo "=== Setting up formal methods tools ==="
echo

# Create tools directory
mkdir -p "$TOOLS_DIR"

# Function to detect OS
detect_os() {
    case "$(uname -s)" in
        FreeBSD)
            echo "freebsd"
            ;;
        Darwin)
            echo "darwin"
            ;;
        Linux)
            echo "linux"
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

OS=$(detect_os)

# Install Lean 4
install_lean() {
    echo "Installing Lean 4 for $OS..."
    
    if [ "$OS" = "freebsd" ]; then
        echo "${YELLOW}Note: Lean on FreeBSD requires Linux compatibility or building from source${NC}"
        echo "Checking for Makefile.lean..."
        
        if [ -f "Makefile.lean" ]; then
            echo "Using Makefile.lean for FreeBSD-specific installation"
            gmake -f Makefile.lean lean-install
        else
            echo "Creating minimal Lean setup for FreeBSD..."
            mkdir -p "$TOOLS_DIR/lean4"
            echo "Please see documentation for FreeBSD Lean installation options"
        fi
    else
        # Download appropriate binary
        case "$OS" in
            darwin)
                LEAN_URL="https://github.com/leanprover/lean4/releases/download/$LEAN_VERSION/lean-4.5.0-darwin.tar.gz"
                LEAN_ARCHIVE="lean-darwin.tar.gz"
                ;;
            linux)
                LEAN_URL="https://github.com/leanprover/lean4/releases/download/$LEAN_VERSION/lean-4.5.0-linux.tar.gz"
                LEAN_ARCHIVE="lean-linux.tar.gz"
                ;;
            *)
                echo "Unsupported OS for Lean installation"
                return 1
                ;;
        esac
        
        echo "Downloading Lean from $LEAN_URL..."
        curl -L -o "$TOOLS_DIR/$LEAN_ARCHIVE" "$LEAN_URL"
        
        echo "Extracting Lean..."
        cd "$TOOLS_DIR" && tar xzf "$LEAN_ARCHIVE"
        
        # Rename to consistent directory
        case "$OS" in
            darwin)
                mv lean-4.5.0-darwin lean4
                ;;
            linux)
                mv lean-4.5.0-linux lean4
                ;;
        esac
        
        # Clean up
        rm -f "$LEAN_ARCHIVE"
        cd ../..
        
        echo "${GREEN}Lean 4 installed successfully!${NC}"
    fi
}

# Install other formal methods tools
install_other_tools() {
    echo
    echo "Other formal methods tools can be installed:"
    echo "- Coq: pkg install coq (FreeBSD) | apt install coq (Debian/Ubuntu)"
    echo "- Agda: pkg install hs-Agda (FreeBSD) | apt install agda (Debian/Ubuntu)"
    echo "- Isabelle: Download from https://isabelle.in.tum.de/"
}

# Main installation
install_lean
install_other_tools

echo
echo "${GREEN}Setup complete!${NC}"
echo "Lean 4 location: $TOOLS_DIR/lean4"
echo "Add to PATH: export PATH=\"\$PATH:$(pwd)/$TOOLS_DIR/lean4/bin\""