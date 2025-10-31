#!/bin/bash
# Build cns-starter minimal package for beginners
# This extracts only STARTER examples and core files

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"
STARTER_DIR="$BUILD_DIR/cns-starter"

echo "🚀 Building CNS Starter Package..."
echo ""

# Clean previous build
rm -rf "$BUILD_DIR"
mkdir -p "$STARTER_DIR"

# Copy core executable
echo "📦 Copying core files..."
cp "$PROJECT_ROOT/cns-run" "$STARTER_DIR/"
chmod +x "$STARTER_DIR/cns-run"

# Copy source
cp -r "$PROJECT_ROOT/src" "$STARTER_DIR/"

# Copy only STARTER examples
echo "📝 Extracting starter examples..."
mkdir -p "$STARTER_DIR/examples"
STARTER_COUNT=0

for file in "$PROJECT_ROOT/examples"/*.cns; do
    if grep -q "^# STARTER" "$file"; then
        cp "$file" "$STARTER_DIR/examples/"
        STARTER_COUNT=$((STARTER_COUNT + 1))
        echo "   ✓ $(basename "$file")"
    fi
done

# Copy minimal docs
echo "📚 Copying documentation..."
cp "$PROJECT_ROOT/QUICKSTART.md" "$STARTER_DIR/" 2>/dev/null || true
cp "$PROJECT_ROOT/examples/python-comparison.md" "$STARTER_DIR/" 2>/dev/null || true

# Create starter-specific README
cat > "$STARTER_DIR/README.md" << 'EOF'
# CNS Starter

**Zero-setup API development. Just run.**

CNS (Causal Narrative Script) is a programming language designed for rapid API development with zero dependencies.

## 🚀 Quick Start

```bash
# Run the killer app demo (multi-API workflow)
./cns-run examples/killer-app-demo.cns

# Try other examples
./cns-run examples/hello.cns
./cns-run examples/fibonacci.cns
./cns-run examples/demo-webserver.cns  # Press Ctrl+C to stop
```

## ✨ What Makes CNS Special?

- **Zero Dependencies** - No pip, npm, or cargo required
- **Instant Execution** - No compilation, no setup
- **Built-in HTTP** - HTTP client and server out of the box
- **Built-in JSON** - Parse JSON natively
- **Self-Documenting** - Code reads like a story
- **LLM-Friendly** - Optimized for AI code generation

## 📦 What's Included

This starter package contains:
- `cns-run` - The CNS interpreter
- `src/` - Core implementation
- `examples/` - Beginner-friendly examples

## 📖 Examples

### Hello World (Loop Example)
```bash
./cns-run examples/hello.cns
```

### Killer App (Multi-API Demo)
```bash
./cns-run examples/killer-app-demo.cns
```
Calls 2 REST APIs, parses JSON, displays results. **42 lines vs 67 in Python!**

### Fibonacci (Algorithm)
```bash
./cns-run examples/fibonacci.cns
```

### Web Server
```bash
./cns-run examples/demo-webserver.cns
# Open http://localhost:8080 in browser
# Press Ctrl+C to stop
```

### HTTP Client
```bash
./cns-run examples/test-http-get.cns
```

## 🎯 Next Steps

1. **Read QUICKSTART.md** - 5-minute tutorial
2. **Read python-comparison.md** - See CNS vs Python
3. **Explore examples/** - Run all starter examples
4. **Visit the main repo** - [github.com/yourname/cns](https://github.com/yourname/cns)

## 🌟 Why CNS?

**Traditional Python:**
```bash
python3 -m venv venv
source venv/bin/activate
pip install requests
python my_api.py
```

**CNS:**
```bash
./cns-run my_api.cns
```

**That's it. No setup. Just works.**

## 📚 Full Documentation

For complete documentation, advanced examples, and development guides:
- [Main CNS Repository](https://github.com/yourname/cns)
- [Full Documentation](https://github.com/yourname/cns/tree/main/docs)
- [All Examples](https://github.com/yourname/cns/tree/main/examples) (50+)

## 🤝 Contributing

Found a bug? Want to contribute? Visit the main repository!

---

**Made with ❤️ for rapid API development**
EOF

# Create version file
cat > "$STARTER_DIR/VERSION" << EOF
CNS Starter Package
Version: 1.0.0
Build Date: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
Starter Examples: $STARTER_COUNT
EOF

# Package it
echo ""
echo "📦 Creating tarball..."
cd "$BUILD_DIR"
tar -czf "cns-starter.tar.gz" cns-starter/

# Show results
TARBALL_SIZE=$(du -h "$BUILD_DIR/cns-starter.tar.gz" | cut -f1)
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ CNS Starter Package Built Successfully!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Package: $BUILD_DIR/cns-starter.tar.gz"
echo "Size: $TARBALL_SIZE"
echo "Starter Examples: $STARTER_COUNT"
echo ""
echo "📤 To distribute:"
echo "   1. Upload to GitHub Releases"
echo "   2. Users download and extract"
echo "   3. Run: cd cns-starter && ./cns-run examples/killer-app-demo.cns"
echo ""
echo "🧪 To test locally:"
echo "   cd $STARTER_DIR"
echo "   ./cns-run examples/killer-app-demo.cns"
echo ""
