# CNS Installation Guides

**Quick Start:** CNS works out of the box with SBCL (Steel Bank Common Lisp). Optional features require additional libraries.

---

## Core Installation

**Minimal setup** (works for most examples):

```bash
# Install SBCL
# Ubuntu/Debian
sudo apt-get install sbcl

# macOS
brew install sbcl

# Clone CNS
git clone https://github.com/jessecrouch/cns
cd cns

# Run your first program
./cns-run examples/hello.cns
```

That's it! Most CNS programs work with just SBCL.

---

## Optional Dependencies

Some advanced features require additional Common Lisp libraries:

| Feature | Required Library | Guide |
|---------|-----------------|-------|
| **HTTPS** | cl+ssl | [INSTALL-HTTPS.md](INSTALL-HTTPS.md) |
| **Regex** | cl-ppcre | [INSTALL-REGEX.md](INSTALL-REGEX.md) |
| **SQLite** | cl-sqlite | [INSTALL-SQLITE.md](INSTALL-SQLITE.md) |

### When Do You Need These?

**You DON'T need them for:**
- Basic CNS programs (hello, fibonacci, loops)
- HTTP (plain HTTP works out of the box)
- File I/O
- String operations (TRIM, UPPERCASE, etc.)
- JSON parsing
- CSV files
- Shell execution
- Git operations
- Most examples in `examples/core/` and `examples/features/`

**You NEED them for:**
- **HTTPS connections** - Requires cl+ssl
- **Regex matching** (MATCHES, EXTRACT) - Requires cl-ppcre
- **SQLite databases** - Requires cl-sqlite

### Quick Check: Do I Have Everything?

```bash
# Test HTTPS (requires cl+ssl)
./cns-run examples/test-https.cns

# Test Regex (requires cl-ppcre)
./cns-run examples/test-regex.cns

# Test SQLite (requires cl-sqlite)
./cns-run examples/test-db-simple.cnsc
```

If these run without errors, you're all set!

---

## Installation Guides

### 1. Core Dependencies

**[INSTALL-DEPENDENCIES.md](INSTALL-DEPENDENCIES.md)**
- SBCL installation for all platforms
- Quicklisp setup (Common Lisp package manager)
- Verifying your installation

### 2. HTTPS Support

**[INSTALL-HTTPS.md](INSTALL-HTTPS.md)**
- Installing cl+ssl library
- Platform-specific SSL setup
- Testing HTTPS connections

**Use case:** Calling REST APIs over HTTPS (GitHub, most modern APIs)

### 3. Regex Support

**[INSTALL-REGEX.md](INSTALL-REGEX.md)**
- Installing cl-ppcre library
- Testing regex matching
- Common regex patterns

**Use case:** Pattern matching in text (emails, phone numbers, validation)

### 4. SQLite Database Support

**[INSTALL-SQLITE.md](INSTALL-SQLITE.md)**
- Installing cl-sqlite library
- Testing database connection
- Basic CRUD operations

**Use case:** Local data persistence (users, logs, caching)

---

## Troubleshooting

### "Package not found" errors

```lisp
; The name "CL-PPCRE" does not designate any package.
```

**Solution:** Install the missing library via Quicklisp (see specific guide above).

### Quicklisp not installed

**Symptoms:** Can't load libraries even after installation.

**Solution:** Follow INSTALL-DEPENDENCIES.md to set up Quicklisp first.

### SBCL version issues

**Minimum version:** SBCL 2.0+

**Check your version:**
```bash
sbcl --version
```

**Upgrade if needed:**
```bash
# Ubuntu/Debian
sudo apt-get update && sudo apt-get install sbcl

# macOS
brew upgrade sbcl
```

---

## Platform-Specific Notes

### Linux (Ubuntu/Debian)

Most libraries install cleanly via Quicklisp. SSL libraries may require system packages:

```bash
sudo apt-get install libssl-dev
```

### macOS

Homebrew makes everything easy. Install SBCL first, then Quicklisp handles the rest.

### Windows

CNS works on Windows via SBCL for Windows. Some libraries (especially cl+ssl) may require additional setup. See specific guides.

---

## Complete Installation (All Features)

For a full-featured CNS installation with all optional libraries:

```bash
# 1. Install SBCL
# (platform-specific - see INSTALL-DEPENDENCIES.md)

# 2. Install Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --quit

# 3. Install all CNS dependencies
sbcl --eval '(ql:quickload :cl+ssl)' \
     --eval '(ql:quickload :cl-ppcre)' \
     --eval '(ql:quickload :cl-sqlite)' \
     --quit

# 4. Clone CNS
git clone https://github.com/jessecrouch/cns
cd cns

# 5. Test everything works
./test-all-examples.sh
```

---

## Docker (Optional)

For a pre-configured environment:

```dockerfile
FROM debian:latest

RUN apt-get update && apt-get install -y \
    sbcl \
    libssl-dev \
    git \
    curl

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --quit

# Install CNS dependencies
RUN sbcl --eval '(ql:quickload :cl+ssl)' \
         --eval '(ql:quickload :cl-ppcre)' \
         --eval '(ql:quickload :cl-sqlite)' \
         --quit

# Clone CNS
RUN git clone https://github.com/jessecrouch/cns /opt/cns

WORKDIR /opt/cns
CMD ["/bin/bash"]
```

**Build and run:**
```bash
docker build -t cns .
docker run -it cns
./cns-run examples/hello.cns
```

---

## Next Steps

After installation:

1. **Try core examples:** `./cns-run examples/core/hello.cns`
2. **Read syntax guide:** `docs/language/SYNTAX.md`
3. **Learn patterns:** `docs/language/COMMON-PATTERNS.md`
4. **Explore examples:** `examples/README.md`

---

## Help & Support

**Issues with installation?**
- Check the specific installation guide for your feature
- Ensure Quicklisp is installed correctly
- Verify SBCL version is 2.0+

**Still stuck?**
- GitHub Issues: https://github.com/jessecrouch/cns/issues
- Include: OS, SBCL version, error message

---

*Last Updated: 2025-11-02*  
*Covers: CNS installation with all optional features*
