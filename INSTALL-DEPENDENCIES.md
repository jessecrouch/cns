# CNS Dependency Installation Guide

**Quick Start:** Install all optional dependencies in one command.

---

## One-Command Install

```bash
# Install all Quicklisp packages at once
sbcl --eval "(ql:quickload :cl+ssl)" \
     --eval "(ql:quickload :flexi-streams)" \
     --eval "(ql:quickload :cl-ppcre)" \
     --eval "(ql:quickload :cl-json)" \
     --quit
```

**Time:** ~2 minutes (one-time setup)  
**Result:** Full HTTPS, Regex, and JSON support

---

## What Gets Installed

### 1. HTTPS Support (cl+ssl + flexi-streams)

**Enables:**
- Secure connections to HTTPS APIs
- GitHub, Stripe, AWS, and other modern APIs
- SSL/TLS encryption

**Test it:**
```bash
cd /home/bolt/Documents/cns
./cns-run examples/test-https.cnsc
```

**Example:**
```cnsc
Story: Fetch GitHub Zen
G: zen:S=""
S1→ Effect: HTTP GET from "https://api.github.com/zen" into zen
S2→ Effect: Print "Zen: {zen}"
E: zen
```

### 2. Regex Support (cl-ppcre)

**Enables:**
- Pattern matching with MATCHES operator
- String extraction with EXTRACT operator
- Capture groups (GROUP n)
- All regex syntax: \d, \w, \s, etc.

**Test it:**
```bash
./cns-run examples/test-regex-simple.cns
```

**Example:**
```cnsc
Story: Extract Phone Number
G: text:S="Call 555-1234", phone:S=""
S1→ phone=EXTRACT "\\d{3}-\\d{4}" FROM text
S2→ Effect: Print "Phone: {phone}"
E: phone
```

### 3. Enhanced JSON (cl-json)

**Enables:**
- Better performance for large JSON
- Advanced JSON generation
- Type-safe parsing

**Note:** CNS already has a built-in JSON parser that handles most cases. This is optional for advanced use.

---

## Troubleshooting

### "Quicklisp not found"

Install Quicklisp first:
```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql:add-to-init-file)" \
     --quit
```

### "Package not loading"

Check Quicklisp is in your SBCL init file:
```bash
cat ~/.sbclrc
# Should contain: (load "~/quicklisp/setup.lisp")
```

### "HTTPS still not working"

Verify packages loaded:
```bash
sbcl --eval "(ql:quickload :cl+ssl)" \
     --eval "(ql:quickload :flexi-streams)" \
     --eval "(format t \"SUCCESS~%\")" \
     --quit
```

---

## Graceful Fallback

CNS works **without** these dependencies:

- **No cl+ssl:** HTTPS falls back to HTTP (with warning)
- **No cl-ppcre:** MATCHES and EXTRACT return nil (with warning)
- **No cl-json:** Built-in parser still works

This means CNS programs run everywhere, even in minimal environments.

---

## Dependencies by Feature

| Feature | Required | Optional | Install Command |
|---------|----------|----------|----------------|
| **HTTP** | None | - | - |
| **HTTPS** | cl+ssl, flexi-streams | - | `(ql:quickload :cl+ssl :flexi-streams)` |
| **Regex** | cl-ppcre | - | `(ql:quickload :cl-ppcre)` |
| **JSON** | None | cl-json | `(ql:quickload :cl-json)` |
| **Database** | sqlite3 CLI | - | `apt-get install sqlite3` |
| **Date/Time** | None | - | - |
| **Files** | None | - | - |
| **Sockets** | None | - | - |

---

## Verification

Test all features:
```bash
cd /home/bolt/Documents/cns

# Test HTTPS
./cns-run examples/test-https.cnsc

# Test Regex  
./cns-run examples/test-regex-simple.cns

# Test Database
./cns-run examples/test-db-simple.cnsc

# Test everything
./tests/run-all-tests.sh
```

---

## Next Steps

- [Quickstart Guide](QUICKSTART.md) - Learn CNS basics
- [Examples](examples/) - 66 example programs
- [Roadmap](docs/development/ROADMAP.md) - What's coming next

---

**Questions?** [Open an issue](https://github.com/jessecrouch/cns/issues)
