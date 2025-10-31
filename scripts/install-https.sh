#!/bin/bash
# Install HTTPS support for CNS
# This script installs Quicklisp and cl+ssl for SBCL

set -e

echo "=================================="
echo "CNS HTTPS Support Installer"
echo "=================================="
echo ""

# Check if SBCL is installed
if ! command -v sbcl &> /dev/null; then
    echo "ERROR: SBCL not found. Install with:"
    echo "  Ubuntu/Debian: sudo apt-get install sbcl"
    echo "  macOS: brew install sbcl"
    echo "  Arch: sudo pacman -S sbcl"
    exit 1
fi

echo "✓ SBCL found: $(sbcl --version | head -1)"

# Check if Quicklisp is already installed
if sbcl --eval "(require 'asdf)" --eval "(handler-case (asdf:load-system 'quicklisp :verbose nil) (error () (sb-ext:exit :code 1)))" --quit &> /dev/null; then
    echo "✓ Quicklisp already installed"
else
    echo "Installing Quicklisp..."
    
    # Download Quicklisp
    if [ -f quicklisp.lisp ]; then
        echo "  Using existing quicklisp.lisp"
    else
        curl -O https://beta.quicklisp.org/quicklisp.lisp
    fi
    
    # Install Quicklisp
    sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
    
    # Add to init file
    sbcl --eval "(load \"~/quicklisp/setup.lisp\")" --eval "(ql:add-to-init-file)" --quit
    
    echo "✓ Quicklisp installed"
fi

# Install cl+ssl
echo "Installing cl+ssl..."
sbcl --eval "(ql:quickload :cl+ssl :verbose nil)" --quit
echo "✓ cl+ssl installed"

echo ""
echo "=================================="
echo "Installation Complete!"
echo "=================================="
echo ""
echo "Test HTTPS support with:"
echo "  ./cns-run examples/test-https.cnsc"
echo ""
echo "If you see a GitHub Zen quote, HTTPS is working!"
