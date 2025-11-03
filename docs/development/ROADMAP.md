# CNS Development Roadmap

**Last Updated:** November 1, 2025  
**Current Version:** v1.7.0 - File Search Operations  
**Current Coverage:** 65% of general-purpose language capabilities  
**Development Velocity:** 10x faster than original plan (7 releases in 6 days)

---

## 🎯 Vision

**CNS is the first general-purpose programming language designed from the ground up for LLM code generation.**

### Core Principles
- **Narrative syntax** that mirrors human reasoning and LLM thought processes
- **Zero dependencies** for rapid deployment
- **100% LLM success rate** - validated code generation on first attempt
- **Self-documenting** - code explains its own reasoning
- **Production-ready** - from prototype to deployment with the same code

### Target Use Cases
1. ✅ **Web backends & REST APIs** (fully supported)
2. ✅ **CLI tools & automation scripts** (90% complete)
3. ✅ **Code analysis & automation agents** (80% complete)
4. 🚧 **Data pipelines & ETL** (in progress)
5. 🚧 **General-purpose programming** (65% complete, ongoing)

---

## 📊 Current State (v1.7.0)

### ✅ Completed Features (Phase A+B - 100%)

**Core Language** (Stable since v1.0)
- Variables with type annotations (Integer, String, List, Map)
- Control flow (If/Otherwise, repeat from, go to)
- Functions with recursion
- Error handling (Error section)
- CNSC compact format (62% code reduction)

**I/O & Networking** (95%)
- ✅ HTTP GET/POST with full HTTPS support (cl+ssl)
- ✅ TCP sockets (full server implementation)
- ✅ File I/O (read, write, append)
- ✅ Console output with expression evaluation
- 🚧 WebSockets (planned Phase D)

**Data Operations** (100%)
- ✅ Strings: split, contains, starts-with, TRIM, UPPERCASE, LOWERCASE, REPLACE, JOIN
- ✅ Multi-part concatenation: `"text" + var + "more"`
- ✅ Lists: add, remove, length, where, iteration
- ✅ Maps: key-value operations
- ✅ JSON: Full parsing (nested objects, arrays, dot notation, all types)
- ✅ CSV: Read/write with headers, list-of-lists and list-of-maps
- ✅ Regex: MATCHES and EXTRACT with capture groups (cl-ppcre)
- ✅ Date/Time: NOW(), TIMESTAMP(), FORMAT TIME, time arithmetic

**Database & Persistence** (100%)
- ✅ SQLite: CONNECT, EXECUTE (DDL/DML), QUERY (SELECT)
- ✅ CSV files with headers
- 🚧 PostgreSQL (planned Phase C)
- 🚧 MySQL (planned Phase C)

**System Integration** (95%)
- ✅ Environment variables (ENV function)
- ✅ Shell execution (SHELL command with output/error/exit-code capture)
- ✅ **File search: FIND** (recursive file discovery by pattern) - v1.7.0
- ✅ **Content search: GREP** (regex search across files) - v1.7.0
- ✅ Basic git operations (STATUS, DIFF, CHECKOUT, ADD, COMMIT, CLONE)
- ✅ Advanced git operations (BRANCH management, unified DIFF, LOG, MERGE)
- 🚧 Command-line arguments (next)
- 🚧 Process management (backgrounding, signals)

**Math & Logic** (85%)
- ✅ Arithmetic: +, -, *, /, %
- ✅ Comparison: >, <, >=, <=, ==, !=
- ✅ Boolean: AND, OR, NOT
- 🚧 Advanced math: SQRT, POW, etc. (planned)

---

## 🚀 Active Development (Phase C - 100% Complete)

**Goal:** Enable automation agents and advanced tooling

**Timeline:** 3-4 weeks total (started Nov 1)  
**Status:** Phase C 100% complete as of v1.7.0 + LLM improvements

### ✅ Completed (Phase C)
- v1.5.0: Shell execution + basic git operations
- v1.6.0: Advanced git operations (branch, log, merge)
- v1.7.0: File search (FIND) + content search (GREP)
- LLM-First Improvements: Error messages, validation, strict mode, trace mode, documentation

### 🚧 Next Steps (Phase D Planning)

#### v1.8.0: CLI Arguments & Process Management (3-5 days)
**Goal:** Complete command-line tool capabilities

**Features:**
1. **Command-line arguments**
   - Positional arguments: `ARGS[0]`, `ARGS[1]`
   - Named arguments: `ARG("--port", "8080")`
   - Flag detection: `HAS_FLAG("--verbose")`

2. **Process management**
   - Background jobs: `SHELL "command" BACKGROUND INTO pid`
   - Process signals: `KILL pid WITH SIGTERM`
   - Wait for completion: `WAIT FOR pid`

3. **File system operations**
   - List files: `LIST FILES IN path INTO files`
   - Delete: `DELETE FILE path`
   - Rename: `RENAME FILE old_path TO new_path`
   - Check existence: `FILE EXISTS path`

**Use Cases:**
- Production CLI tools
- Build automation scripts
- System administration tasks
- Process orchestration

#### v1.9.0: Data Processing Enhancement (3-5 days)
**Goal:** Better data manipulation capabilities

**Features:**
1. **Advanced list operations**
   - Sort: `SORT items BY field`
   - Reverse: `REVERSE items`
   - Unique: `UNIQUE items`
   - Slice: `SLICE items FROM 0 TO 10`

2. **Map operations**
   - Keys: `KEYS OF config`
   - Values: `VALUES OF config`
   - Merge: `MERGE map1 WITH map2`

3. **String operations**
   - Padding: `PAD text TO 10 WITH " "`
   - Strip: `STRIP "[]" FROM text`
   - URL encode/decode: `URL_ENCODE text`

**Use Cases:**
- Data transformation pipelines
- Report generation
- API data processing
- Configuration management

---

## 🌟 Phase D: Production Polish (v1.8-2.0)

**Timeline:** 2-3 months  
**Coverage:** 65% → 80%

### Priority Features

**CLI & System (2 weeks)**
1. Command-line arguments parsing
2. File system operations (LIST_FILES, DELETE, RENAME, etc.)
3. Math helpers (SQRT, POW, ABS, ROUND, RANDOM)

**Advanced Data (1 week)**
4. Better list operations (SORT, REVERSE, UNIQUE, SLICE)
5. Map/dictionary operations (KEYS, VALUES, MERGE)
6. Advanced string operations (PAD, STRIP, URL_ENCODE/DECODE)

**Security & Encoding (1 week)**
7. Hashing & crypto (SHA256, HMAC, BASE64, UUID)
8. Process management (background jobs, signals)

**Polish (2 weeks)**
9. Better error messages
10. Performance optimization
11. Documentation improvements
12. More comprehensive examples

---

## 🏆 Phase E: Ecosystem Maturity (v3.0+)

**Timeline:** 3-6 months  
**Coverage:** 80% → 90%+

### Advanced Features
- Compression (ZIP, GZIP, TAR)
- XML/YAML parsing
- WebSockets
- Async/Concurrency primitives
- Template engine (HTML, text)
- Logging framework
- Testing framework for CNS code
- Package manager for CNS libraries

---

## 📈 Development Velocity

### Actual Progress (Last 7 Days)

**v1.1.0** (Oct 30): JSON + ENV  
**v1.2.0** (Oct 31): Regex + Date/Time  
**v1.3.0** (Oct 31): SQLite database  
**v1.4.0** (Nov 1): String helpers + CSV  
**v1.5.0** (Nov 1): Shell + basic git  
**v1.6.0** (Nov 1): Advanced git operations  
**v1.7.0** (Nov 1): File search (FIND + GREP)  

**Total:** 7 major releases in 6 days  
**Original plan:** 5-6 weeks for same features  
**Result:** **10x faster than planned!**

### Why So Fast?
1. Established patterns (graceful fallback, CNSC-first, comprehensive examples)
2. Battle-tested interpreter architecture
3. Zero external dependencies = no integration hell
4. Clear roadmap = no analysis paralysis
5. LLM-assisted development (eating our own dog food)

## 🎯 Phase E: Real-World Applications (v2.0-2.5)

**Timeline:** 2-4 months  
**Coverage:** 80% → 90%

### Strategy: Build Production Apps

Instead of benchmark competitions, focus on building 10+ real production applications to validate CNS in actual use cases:

**Categories:**
1. **Web Services** (3-4 apps)
   - REST API with authentication
   - WebSocket chat server
   - Static file server with routing
   - Webhook processor

2. **CLI Tools** (3-4 apps)
   - Log analyzer
   - Configuration generator
   - Deployment automation
   - Code formatter/linter

3. **Data Processing** (2-3 apps)
   - ETL pipeline
   - Report generator
   - Data validator
   - Metrics aggregator

**Goals:**
- Identify missing features through real usage
- Build library of reusable patterns
- Create production deployment guides
- Gather community feedback
- Validate 90% language coverage claim

---

## 🎓 Development Standards

### CNSC-First Policy (v1.1.0+)

**All new examples and agent code should be written in CNSC format first.**

**Why:**
- 62% code reduction = better LLM context
- Faster generation = fewer errors
- More examples fit in prompts
- Better for training data

**Guidelines:**
- New examples >30 lines: `.cnsc` primary, `.cns` optional
- Agent code: `.cnsc` only
- Test cases: `.cnsc`
- Beginner tutorials: `.cns` (explicit syntax easier to learn)
- Complex examples: Both formats

**Tools:**
- `./cns-run` accepts both formats seamlessly
- `./cns-expand file.cnsc` generates verbose CNS
- `./cns-validate` works with both

---

## 🛠️ Implementation Strategy

### 1. Leverage Common Lisp Libraries
Don't reinvent the wheel:
- CL+SSL (HTTPS)
- CL-PPCRE (Regex)
- CLSQL (Databases)
- Ironclad (Crypto - future)

### 2. Maintain Minimal Dependencies
- Bundle libraries into CNS distribution
- Graceful fallback when optional libs missing
- Starter package stays <100KB
- Full distribution <50MB

### 3. LLM-First Design
Every new feature must:
- Have clear, unambiguous syntax
- Work on first LLM generation attempt
- Include narrative examples
- Support CNSC compact format

### 4. Backward Compatibility
- Never break existing code
- Deprecation warnings for 2+ versions
- Migration guides for breaking changes

---

## 📚 Documentation Structure

### User Documentation
- **QUICKSTART.md** - 5-minute tutorial
- **README.md** - Project overview
- **examples/** - 79 working examples

### Development Documentation
- **PROJECT-STATUS.md** - Current state tracking
- **ROADMAP.md** - This document
- **RELEASE-NOTES-v*.md** - Detailed release notes
- **docs/guides/** - Feature-specific guides

### For Each Feature
1. Working examples (3-5 minimum)
2. Syntax documentation
3. LLM prompt templates
4. Test coverage

---

## 🛠️ Development Environment

### Installed Language Toolchains

**Rust** - v1.91.0 ✅
- Installed via rustup
- Location: `~/.cargo/bin/`
- Usage: `cargo build`, `cargo test`

**Go** - v1.25.3 ✅  
- Pre-installed (latest)
- Usage: `go test ./...`, `go build`

**Java** - OpenJDK 11 ✅
- Installed via apt
- Includes: maven (build tool)

**C/C++** - GCC 11.4.0 ✅
- Pre-installed
- Usage: `make`, `gcc`, `g++`

**Python** - v3.10+ ✅
- Pre-installed
- Standard library available

**Node/TypeScript** - v12.22.9 ✅
- Pre-installed
- npm package manager available

**Why This Matters:**
- Can test multi-language integrations locally
- Shell command examples work across languages
- Fast iteration cycle (build/test in seconds)
- Validates CNS works with diverse toolchains

**Note:** Add `~/.cargo/bin` to PATH for Rust:
```bash
export PATH="$HOME/.cargo/bin:$PATH"
```

---

## 🎯 Immediate Next Steps

### This Week (v1.8.0 Planning)

**CLI Arguments & Process Management:**
- Design command-line argument API
- Implement argument parsing
- Add process management primitives
- Create comprehensive examples
- **Target:** 3-5 days implementation

### Next 2 Weeks (v1.9.0 Planning)

**Data Processing Enhancement:**
- Advanced list operations (sort, reverse, unique, slice)
- Map operations (keys, values, merge)
- Enhanced string operations (pad, strip, URL encode)
- **Target:** 3-5 days implementation

### Month 2-3 (v2.0.0 Planning)

**Production Applications:**
- Build 3-4 real web services
- Build 3-4 production CLI tools
- Build 2-3 data processing apps
- Gather community feedback
- Identify missing features
- Create deployment guides
- **Target:** 2-3 months of real-world validation

---

## 📞 Contributing

**We welcome contributions!**

**High-value areas:**
- More examples (especially real-world use cases)
- Testing with different LLMs (Claude, GPT-4, Gemini)
- Bug reports and feature requests
- Documentation improvements

**Where to start:**
- GitHub Issues for bugs/features
- GitHub Discussions for design questions
- examples/ for community contributions

---

## 🎨 Long-Term Vision

**Year 1 Goal:** 90% language coverage, 10k+ GitHub stars, production adoption

**Year 2 Goal:** Enterprise use cases, training dataset, ecosystem growth

**Year 3 Goal:** First general-purpose language where LLMs write better code than humans

**The Narrative:**
> "CNS isn't just readable code - it's code that thinks like humans and LLMs think. By focusing on narrative clarity and zero dependencies, CNS enables reliable automated code generation at scale."

---

**Maintained by:** Jesse Crouch  
**License:** MIT  
**Repository:** https://github.com/jessecrouch/cns
