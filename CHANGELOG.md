# CNS Changelog

All notable changes to the CNS (Cause-and-Effect Narrative Script) language.

**Format:** Based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)  
**Versioning:** [Semantic Versioning](https://semver.org/spec/v2.0.0.html)

---

## [Unreleased]

### In Progress
- Strict mode NIL enforcement
- Expression parsing improvements (literal-first, multi-operator)
- LLM-first repository reorganization
- Enhanced validator (control flow analysis)

---

## [1.7.0] - 2025-11-01

### Added
- **FIND command**: Recursive file discovery with glob patterns (`*.cns`, `test-*`)
- **GREP command**: Content search with regex across single/multiple files
- **Multi-part string concatenation**: `"text" + var + "more"` in expressions
- File counting with `WITH COUNT variable` clause
- Rich match data: `[file, line_number, text]` for GREP results
- Examples: `test-find-basic.cns`, `test-grep-basic.cns`, `test-code-navigation.cns`

### Impact
- Phase C 50% complete (automation foundation)
- Native code navigation without SHELL dependencies
- 80% of automated agent workflow implementable in pure CNS

---

## [1.6.0] - 2025-11-01

### Added
- **GIT DIFF**: Unified diff format for patch generation
- **GIT BRANCH**: List, create, and delete branches programmatically
- **GIT LOG**: Commit history inspection with flexible formatting
- **GIT MERGE**: Branch merging with conflict detection

### Fixed
- INTO clause parsing in all git commands
- Branch name handling with special characters

### Impact
- Phase C 40% complete
- Complete git workflow for automated agents
- Essential for patch generation and CI/CD automation

---

## [1.5.0] - 2025-11-01

### Added
- **SHELL command**: Execute shell commands with full I/O capture
- **Git operations**: STATUS, DIFF, CHECKOUT, ADD, COMMIT, CLONE
- Output capture: stdout, stderr, exit code
- Timeout support for long-running commands
- Examples: `test-shell.cns`, `test-git-basic.cns`, `test-git-workflow.cns`

### Impact
- Phase C initiated (Automation Track)
- External tool integration enabled
- Foundation for automation agents

---

## [1.4.0] - 2025-11-01

### Added
- **String helpers**: TRIM, UPPERCASE, LOWERCASE, REPLACE, JOIN, LENGTH_OF
- **CSV support**: READ CSV, WRITE CSV with headers
- List-of-maps and list-of-lists CSV formats
- Header row detection and manipulation

### Fixed
- List literal parsing edge cases
- String concatenation in complex expressions

### Impact
- Phase B 100% complete (exceeded target)
- Production-ready data processing
- 60% overall language coverage

---

## [1.3.0] - 2025-10-31

### Added
- **SQLite database**: CONNECT, EXECUTE, QUERY commands
- Shell-based wrapper (zero Lisp dependencies)
- Transaction support via SQL
- Full CRUD operations
- Examples: `test-db-simple.cnsc`, `test-db-comprehensive.cnsc`

### Impact
- Data-driven applications enabled
- Phase B 80% complete
- 55% language coverage

### Installation
- See `docs/install/INSTALL-SQLITE.md` for setup

---

## [1.2.0] - 2025-10-31

### Added
- **Regex support**: MATCHES and EXTRACT operators
- **Date/Time operations**: NOW(), TIMESTAMP(), FORMAT TIME
- Time arithmetic and formatting
- cl-ppcre integration with graceful fallback
- Capture groups in EXTRACT
- Examples: `test-regex.cns`, `test-datetime.cns`

### Impact
- Text processing and scheduling enabled
- Phase B 65% complete
- 50% language coverage

### Installation
- See `docs/install/INSTALL-REGEX.md` for cl-ppcre setup

---

## [1.1.0] - 2025-10-30

### Added
- **Enhanced JSON parser**: Nested objects, arrays, dot notation
- Array indexing: `items[0]`, `users[2].email`
- Dot notation: `user.profile.name`
- **Environment variables**: `ENV("VAR_NAME", "default")`
- All JSON types: string, number, boolean, null, object, array
- LENGTH operator for arrays and objects
- Examples: `test-json-nested.cns`, `test-env-vars.cns`

### Impact
- Modern API integration ready
- Phase B 40% complete
- 45% language coverage

---

## [1.0.0] - 2025-10-27

### Added - Core Language
- Variables with type annotations (Integer, String, List, Map)
- Control flow: If/Otherwise, repeat from, go to
- Functions with recursion support
- Error handling (Error section)
- CNSC compact format (62% code reduction)

### Added - I/O & Networking
- HTTP GET/POST with HTTPS support (cl+ssl)
- TCP socket server implementation
- File I/O: read, write, append
- Console output with interpolation

### Added - Data Structures
- Strings: split, contains, starts-with, escape sequences
- Lists: add, remove, length, where, iteration
- Maps: basic key-value operations
- Basic JSON parsing

### Added - Math & Logic
- Arithmetic: +, -, *, /, %
- Comparison: >, <, >=, <=, ==, !=
- Boolean: AND, OR, NOT

### Documentation
- QUICKSTART.md - 5-minute tutorial
- 30+ working examples
- CNSC syntax guide

### Impact
- 20% general-purpose language coverage
- 100% LLM success rate on test suite
- Zero-dependency execution (cl+ssl optional)
- Narrative programming thesis validated

---

## Development Velocity

**7 releases in 6 days** (v1.1.0 through v1.7.0)
- **10x faster than original roadmap**
- Achieved through: Established patterns, zero-dependency approach, focused scope

**Phase Progress:**
- Phase A (Foundation): ✅ Complete
- Phase B (Backend Features): ✅ 100% Complete (Nov 1, 2025)
- Phase C (Benchmark Track): 🚧 50% Complete (In Progress)

---

## Upgrade Notes

### Backward Compatibility
**All versions are 100% backward compatible.** No breaking changes have been introduced.

### Recommended Upgrades
When upgrading to v1.7.0 from any earlier version:
- Use FIND/GREP instead of SHELL-based file search (faster, cleaner)
- Use native JSON parsing instead of manual string operations
- Use ENV() for configuration instead of hardcoded values
- Use string helpers (TRIM, UPPERCASE) instead of SHELL commands

### Migration Path
All features are additive. Existing programs continue to work without modification.

---

## Version Format

**Major.Minor.Patch** - Following [Semantic Versioning](https://semver.org/)

- **Major**: Breaking changes (none yet!)
- **Minor**: New features, additive changes
- **Patch**: Bug fixes, documentation updates

---

## Links

- **Documentation**: `docs/` directory
- **Examples**: `examples/` directory  
- **Guides**: `docs/guides/`
- **Installation**: `docs/install/`
- **Development**: `docs/development/`

---

**Maintained by:** CNS Development Team  
**License:** MIT  
**Last Updated:** November 2, 2025
