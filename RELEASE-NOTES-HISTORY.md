# CNS Release Notes - Historical Archive

This document contains summarized release notes for v1.1.0 through v1.5.0. For detailed release notes, see:
- **RELEASE-NOTES-v1.7.0.md** - Current release (File Search Operations)
- **RELEASE-NOTES-v1.6.0.md** - Advanced Git Operations

---

## v1.5.0 - Shell Execution & Git Operations (Nov 1, 2025)

**Phase C Initiated: Benchmark Track begins**

### Features Added
- **SHELL command**: Execute shell commands with full I/O capture (stdout, stderr, exit code)
- **Basic git operations**: STATUS, DIFF, CHECKOUT, ADD, COMMIT, CLONE
- Error handling and timeout support

### Impact
- Phase C 20% complete
- Foundation for SWE-Bench agent development
- Enables external tool integration

### Examples
- `test-shell.cns` - Shell command execution
- `test-git-basic.cns` - Basic git workflow
- `test-git-workflow.cns` - Complete development workflow

---

## v1.4.0 - String Helpers & CSV Support (Nov 1, 2025)

**Phase B Complete: 100% of planned Phase B features delivered**

### Features Added
- **String helpers**: TRIM, UPPERCASE, LOWERCASE, REPLACE, JOIN, LENGTH_OF
- **CSV support**: READ CSV, WRITE CSV with headers
- **List literal parsing**: Fixed edge cases

### Impact
- Phase B 100% complete (target exceeded)
- 60% overall language coverage
- Production-ready data processing

### Examples
- `test-string-helpers.cns` - String operations
- `test-csv.cns` - CSV read/write with headers

---

## v1.3.0 - SQLite Database Support (Oct 31, 2025)

**Database persistence unlocked**

### Features Added
- **SQLite integration**: CONNECT, EXECUTE, QUERY commands
- **Shell-based wrapper**: Zero Lisp dependencies (graceful fallback)
- Transaction support via SQL

### Impact
- 55% language coverage
- Enables data-driven applications
- Phase B 80% complete

### Examples
- `test-db-simple.cnsc` - Basic database operations
- `test-db-comprehensive.cnsc` - Full CRUD workflow

### Installation
See INSTALL-SQLITE.md for setup instructions

---

## v1.2.0 - Regex & Date/Time Operations (Oct 31, 2025)

**Pattern matching and temporal operations**

### Features Added
- **Regex support**: MATCHES and EXTRACT operators with capture groups
- **Date/Time**: NOW(), TIMESTAMP(), FORMAT TIME, time arithmetic
- **cl-ppcre integration**: Full regex engine with graceful fallback

### Impact
- 50% language coverage
- Phase B 65% complete
- Text processing and scheduling enabled

### Examples
- `test-regex.cns`, `test-regex-simple.cns` - Pattern matching
- `test-datetime.cns`, `test-datetime-simple.cns` - Time operations

### Installation
See INSTALL-REGEX.md for cl-ppcre setup

---

## v1.1.0 - Enhanced JSON & Environment Variables (Oct 30, 2025)

**Foundation for modern API development**

### Features Added
- **Enhanced JSON parser**: Nested objects, arrays, dot notation (user.profile.name)
- **Array indexing**: items[0], users[2].email
- **Environment variables**: ENV("VAR_NAME", "default")
- **All JSON types**: string, number, boolean, null, object, array
- **LENGTH operator**: For arrays and objects

### Impact
- 45% language coverage
- Phase B 40% complete
- Modern API integration ready

### Examples
- `test-json-nested.cns` - Nested object access
- `test-json-comprehensive.cns` - Full JSON feature set
- `test-env-vars.cns` - Environment variable usage

### Implementation
- Custom recursive parser (no external dependencies)
- Graceful error handling
- Support for complex nested structures

---

## v1.0.0 - Initial Release (Oct 27, 2025)

**The foundation: HTTP Client & Core Language**

### Core Features
- **Core language**: Variables, control flow, functions, error handling
- **HTTP client**: GET/POST with HTTPS support (cl+ssl)
- **TCP sockets**: Full server implementation
- **File I/O**: Read, write, append
- **Data structures**: Strings, lists, maps, basic JSON
- **Math & logic**: Arithmetic, comparison, boolean operations

### Achievement
- 20% general-purpose language coverage
- 100% LLM success rate on test suite
- Zero-dependency execution (optional cl+ssl for HTTPS)
- Narrative syntax validates core thesis

### Documentation
- QUICKSTART.md - 5-minute tutorial
- 30+ working examples
- Starter package (34KB download)

---

## Development Velocity Analysis

**Completed in 6 days:**
- v1.1.0: JSON + ENV (1 day) - planned 5-6 days
- v1.2.0: Regex + Date/Time (1 day) - planned 2 days
- v1.3.0: SQLite (3 hours) - planned 3-5 days
- v1.4.0: String helpers + CSV (4 hours) - planned 2 days
- v1.5.0: Shell + Git (1 day) - planned 1 week
- v1.6.0: Advanced Git (2 days) - unplanned (ahead of schedule)
- v1.7.0: File Search (1 day) - unplanned (ahead of schedule)

**Result: 10x faster than original roadmap**

**Why:**
- Established patterns (graceful fallback, CNSC-first)
- Battle-tested architecture
- Zero external dependency complexity
- Clear roadmap and focused scope

---

## Migration Guide Summary

### v1.1.0 → v1.7.0 (Cumulative)

**No breaking changes** across any version. All features are additive.

**New capabilities available:**
1. Nested JSON parsing with dot notation
2. Environment variables for configuration
3. Regex pattern matching and extraction
4. Date/time operations and formatting
5. String helper functions (TRIM, UPPERCASE, etc.)
6. CSV file support
7. SQLite database operations
8. Shell command execution
9. Git operations (basic and advanced)
10. File search (FIND) and content search (GREP)

**Recommended upgrades:**
- Use FIND/GREP instead of SHELL-based file search
- Use native JSON parsing instead of manual string operations
- Use ENV() for configuration instead of hardcoded values
- Use GREP for text search instead of manual regex in SHELL

---

## Documentation Updates

**For detailed information on any feature, see:**
- Feature-specific documentation in `docs/guides/`
- Working examples in `examples/`
- Test files in `examples/test-*.cns`
- Current release notes: RELEASE-NOTES-v1.7.0.md

**Development documentation:**
- ROADMAP.md - Current plan and vision
- PROJECT-STATUS.md - Live project tracking
- docs/development/ - Implementation details

---

**Maintained by:** Jesse Crouch  
**License:** MIT  
**Last Updated:** November 1, 2025
