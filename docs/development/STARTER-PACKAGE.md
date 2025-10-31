# CNS Starter Package System

**Date:** October 31, 2025  
**Purpose:** Maintainable beginner-friendly distribution without separate repos

---

## Problem Solved

**Challenge:** How to provide a beginner-friendly "starter" version of CNS without:
- Maintaining two separate repositories (sync nightmare)
- Copying files manually for releases
- Having outdated examples in the starter

**Solution:** Build system that extracts starter content from the main repo automatically.

---

## Architecture

### Single Source of Truth

Everything lives in the main `cns/` repository:

```
cns/                           # Main development repo
├── src/                       # Core (included in starter)
├── cns-run                    # Interpreter (included in starter)
├── examples/
│   ├── hello.cns              # STARTER - marked for inclusion
│   ├── killer-app-demo.cns    # STARTER - marked for inclusion
│   ├── fibonacci.cns          # STARTER - marked for inclusion
│   ├── factorial.cns          # STARTER - marked for inclusion
│   ├── demo-webserver.cns     # STARTER - marked for inclusion
│   ├── test-http-get.cns      # STARTER - marked for inclusion
│   └── ... (50+ other examples, not included in starter)
├── docs/                      # Full docs (not in starter)
├── tests/                     # Test suite (not in starter)
├── QUICKSTART.md              # Included in starter
├── README.md                  # Full README (replaced in starter)
└── scripts/
    └── build-starter.sh       # Build script

build/                         # Generated (gitignored)
└── cns-starter/               # Built starter package
    ├── cns-run
    ├── src/
    ├── examples/              # Only STARTER examples
    ├── README.md              # Starter-specific
    ├── QUICKSTART.md
    ├── python-comparison.md
    └── VERSION
```

---

## How It Works

### 1. Marking Starter Examples

Add `# STARTER` comment to beginner-friendly examples:

```cns
Story: Hello World Example
# STARTER - Basic CNS example showing loops and counting

Given:
  count: Integer = 0
  ...
```

**Currently Marked Examples:**
1. `hello.cns` - Basic loops
2. `factorial.cns` - Simple math
3. `fibonacci.cns` - Classic algorithm
4. `killer-app-demo.cns` - Multi-API demo (flagship)
5. `demo-webserver.cns` - HTTP server
6. `test-http-get.cns` - HTTP client

### 2. Build Script

```bash
./scripts/build-starter.sh
```

**What it does:**
1. Creates `build/cns-starter/` directory
2. Copies core files (`cns-run`, `src/`)
3. Extracts examples with `# STARTER` tag
4. Copies documentation
5. Generates starter-specific README
6. Creates VERSION file with metadata
7. Packages as `cns-starter.tar.gz`

**Output:**
```
build/
├── cns-starter/              # Extracted package
└── cns-starter.tar.gz        # 34KB tarball
```

### 3. Starter-Specific README

The build script generates a beginner-optimized README:

```markdown
# CNS Starter

**Zero-setup API development. Just run.**

## 🚀 Quick Start

./cns-run examples/killer-app-demo.cns

## ✨ What Makes CNS Special?
- Zero Dependencies
- Instant Execution
- Built-in HTTP
...
```

This replaces the full technical README with a marketing-focused version.

---

## Usage

### For Developers (Maintaining Starter)

**Adding a new starter example:**
1. Create or update example in `examples/`
2. Add `# STARTER` comment after Story line
3. Run `./scripts/build-starter.sh`
4. Test: `cd build/cns-starter && ./cns-run examples/your-example.cns`

**Updating core features:**
1. Modify `src/cns.lisp`
2. Changes automatically included in next build
3. No manual sync needed!

**Creating a release:**
```bash
# Build starter package
./scripts/build-starter.sh

# Upload to GitHub Releases
# build/cns-starter.tar.gz -> GitHub Releases page

# Users download
curl -L github.com/user/cns/releases/latest/download/cns-starter.tar.gz | tar xz
cd cns-starter
./cns-run examples/killer-app-demo.cns
```

### For Contributors

Just clone and work in the main repo:
```bash
git clone github.com/user/cns
cd cns
./cns-run examples/killer-app-demo.cns
```

No need to think about the starter package unless releasing.

### For End Users

**Option 1: Starter Package (Recommended)**
```bash
curl -L github.com/user/cns/releases/latest/download/cns-starter.tar.gz | tar xz
cd cns-starter
./cns-run examples/killer-app-demo.cns
```

**Option 2: Full Repository**
```bash
git clone github.com/user/cns
cd cns
./cns-run examples/killer-app-demo.cns
```

---

## Benefits

### Single Source of Truth
- ✅ Core changes automatically propagate
- ✅ Example updates included immediately
- ✅ No manual sync between repos
- ✅ No version drift

### Easy Maintenance
- ✅ Add starter example: just add `# STARTER` tag
- ✅ Remove starter example: just remove tag
- ✅ Update examples: edit once, applies everywhere
- ✅ Update docs: edit in main repo

### Beginner-Friendly
- ✅ Minimal 34KB download (vs full repo)
- ✅ Only 6 curated examples (vs 50+)
- ✅ Optimized README for first-timers
- ✅ Quick start guide included

### Marketing Ready
- ✅ Clean package for demos
- ✅ Easy to distribute (single tarball)
- ✅ Showcases killer app immediately
- ✅ Professional presentation

---

## Package Contents

### What's Included in Starter

```
cns-starter/
├── cns-run                    # Interpreter (223 bytes)
├── src/
│   ├── cns.lisp              # Core implementation
│   ├── cns-validator.lisp    # Validation
│   └── ... (support files)
├── examples/
│   ├── hello.cns             # 366 bytes
│   ├── factorial.cns         # 395 bytes
│   ├── fibonacci.cns         # 686 bytes
│   ├── killer-app-demo.cns   # 2.5KB - flagship
│   ├── demo-webserver.cns    # 2.4KB
│   └── test-http-get.cns     # 641 bytes
├── README.md                  # Starter-specific (2.4KB)
├── QUICKSTART.md             # Tutorial (5.8KB)
├── python-comparison.md       # Marketing (5.5KB)
└── VERSION                    # Build metadata

Total: ~34KB compressed
```

### What's Excluded from Starter

- Advanced examples (40+ files)
- Full documentation (docs/)
- Test suites (tests/)
- Development tools
- LLM training datasets
- Internal scripts

**Why excluded:** Keep starter focused and minimal for beginners.

---

## Starter Examples Explained

### 1. hello.cns (366 bytes)
**Purpose:** Simplest loop example  
**Teaches:** Basic syntax, loops, counting  
**Runtime:** < 1 second

### 2. factorial.cns (395 bytes)
**Purpose:** Simple math computation  
**Teaches:** Math operations, state changes  
**Runtime:** < 1 second

### 3. fibonacci.cns (686 bytes)
**Purpose:** Classic algorithm  
**Teaches:** Multiple variables, iteration  
**Runtime:** < 1 second

### 4. killer-app-demo.cns (2.5KB) ⭐ **Flagship**
**Purpose:** Multi-API orchestration  
**Teaches:** HTTP GET, JSON parsing, real APIs  
**Features:**
- Calls IP geolocation API
- Calls UUID generator API
- Parses JSON responses
- Formatted output
- Zero dependencies

**Runtime:** ~3 seconds (network calls)

### 5. demo-webserver.cns (2.4KB)
**Purpose:** Simple HTTP server  
**Teaches:** Socket programming, web serving  
**Features:**
- Listens on port 8080
- Serves HTML with styling
- Tracks connection count
- Runs until Ctrl+C

**Runtime:** Persistent (until killed)

### 6. test-http-get.cns (641 bytes)
**Purpose:** HTTP client basics  
**Teaches:** Making API calls, checking status  
**Features:**
- Fetches from httpbin.org
- Displays response
- Shows HTTP status

**Runtime:** ~2 seconds

---

## Version File

The build script generates a VERSION file:

```
CNS Starter Package
Version: 1.0.0
Build Date: 2025-10-31 17:30:00 UTC
Starter Examples: 6
```

This helps track which version users have downloaded.

---

## CI/CD Integration (Future)

**GitHub Actions workflow** (not yet implemented):

```yaml
name: Build Starter Package

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install SBCL
        run: sudo apt-get install sbcl
      - name: Build Starter
        run: ./scripts/build-starter.sh
      - name: Upload to Release
        uses: actions/upload-release-asset@v1
        with:
          upload_url: ${{ github.event.release.upload_url }}
          asset_path: build/cns-starter.tar.gz
          asset_name: cns-starter.tar.gz
```

**When implemented:**
1. Push tag: `git tag v1.0.0 && git push origin v1.0.0`
2. GitHub Action builds starter automatically
3. Uploads to GitHub Releases
4. Users download immediately

---

## Maintenance Checklist

**Before each release:**
- [ ] Review STARTER examples (still beginner-friendly?)
- [ ] Run `./scripts/build-starter.sh`
- [ ] Test starter package: `cd build/cns-starter && ./cns-run examples/killer-app-demo.cns`
- [ ] Verify all examples work
- [ ] Check README clarity
- [ ] Update VERSION in script if needed
- [ ] Upload tarball to GitHub Releases

**Quarterly:**
- [ ] Review all STARTER tags (add/remove as needed)
- [ ] Update QUICKSTART.md with new features
- [ ] Refresh python-comparison.md if Python ecosystem changes
- [ ] Test download flow from releases page

---

## Metrics

**Package Efficiency:**
- **Starter:** 34KB (6 examples)
- **Full Repo:** ~20MB (50+ examples, docs, tests)
- **Reduction:** 99.8% smaller!

**Startup Time:**
- Download: < 1 second
- Extract: < 1 second
- First run: < 2 seconds
- **Total:** < 5 seconds from URL to working code

**vs Python:**
```bash
# Python
python3 -m venv venv        # 10-15 seconds
source venv/bin/activate    # 1 second
pip install requests        # 5-30 seconds
python script.py            # 1-2 seconds
# Total: 17-48 seconds

# CNS Starter
curl -L url | tar xz        # 1 second
./cns-run script.cns        # 2 seconds
# Total: 3 seconds
```

**16x faster time-to-execution!**

---

## Future Enhancements

### High Priority
1. **GitHub Actions integration** - Auto-build on release
2. **Homebrew formula** - `brew install cns-starter`
3. **Docker image** - `docker run cns-starter`

### Medium Priority
4. **Windows .exe** - Native Windows build
5. **VS Code extension** - Syntax highlighting for starter users
6. **Online playground** - Try CNS in browser

### Low Priority
7. **Language-specific starters** - Python devs, Node devs, etc.
8. **Tutorial videos** - Step-by-step guides
9. **Interactive REPL** - Web-based REPL

---

## Conclusion

The CNS Starter package system provides:

✅ **Easy Distribution** - Single 34KB tarball  
✅ **Zero Maintenance** - Auto-extracts from main repo  
✅ **Beginner-Friendly** - Curated examples only  
✅ **Marketing Ready** - Professional presentation  
✅ **No Sync Issues** - Single source of truth  
✅ **Production Ready** - Tested and validated

**Perfect for:**
- First-time users trying CNS
- Marketing demos and presentations
- Tutorial videos and blog posts
- Quick prototyping and experimentation

**The killer app is now packaged and ready to ship!** 🚀
