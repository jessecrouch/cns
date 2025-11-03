# HTTPS Support Installation Guide

CNS v1.1.0+ includes HTTPS support via the `cl+ssl` Common Lisp library.

## Quick Install (Recommended)

```bash
# 1. Install Quicklisp (if not already installed)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit

# 2. Add Quicklisp to SBCL init file
sbcl --eval "(ql:add-to-init-file)" --quit

# 3. Install cl+ssl
sbcl --eval "(ql:quickload :cl+ssl)" --quit

# 4. Verify HTTPS works
./cns-run examples/test-https.cnsc
```

## What Happens Without cl+ssl?

CNS gracefully degrades to HTTP-only mode:
- HTTPS URLs automatically fall back to HTTP with a warning
- All HTTP functionality works normally
- No crashes or errors

## Manual Installation

If you prefer to install cl+ssl manually:

### Ubuntu/Debian
```bash
sudo apt-get install libssl-dev
sbcl --eval "(ql:quickload :cl+ssl)" --quit
```

### macOS
```bash
brew install openssl
sbcl --eval "(ql:quickload :cl+ssl)" --quit
```

### Arch Linux
```bash
sudo pacman -S openssl
sbcl --eval "(ql:quickload :cl+ssl)" --quit
```

## Testing HTTPS

After installation, test with:

```bash
# Full CNS syntax
./cns-run examples/test-https.cns

# Compact CNSC syntax
./cns-run examples/test-https.cnsc
```

Expected output:
```
Story: Test HTTPS Support
...
GitHub Zen: [Random Zen quote from GitHub API]
```

## Troubleshooting

### Error: "HTTPS support unavailable"
- Install Quicklisp (see Quick Install above)
- Run: `sbcl --eval "(ql:quickload :cl+ssl)" --quit`

### Error: "SSL certificate verification failed"
- Update OpenSSL: `brew upgrade openssl` (macOS) or `sudo apt update && sudo apt upgrade libssl-dev` (Linux)

### Still not working?
- Open an issue at: https://github.com/yourusername/cns/issues
- Include OS, SBCL version (`sbcl --version`), and error messages

## Status

- ✅ HTTP GET/POST (works without cl+ssl)
- ✅ HTTPS GET/POST (requires cl+ssl)
- ✅ Automatic fallback to HTTP
- ✅ Custom headers support
- ✅ JSON payloads

## Coming Soon (Phase B)

- Environment variables: `ENV("API_KEY")`
- Better JSON parsing: dot notation, nested objects
- Regex support: `REGEX MATCH pattern IN text`
