# CNS Changelog

All notable changes to the CNS (Cause-and-Effect Narrative Script) language.

**Format:** Based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)  
**Versioning:** [Semantic Versioning](https://semver.org/spec/v2.0.0.html)

---

## [Unreleased]

### Planned for Future Releases
- Production polish and performance optimization
- Advanced error recovery mechanisms
- Performance profiling and optimization

---

## [2.0.0] - 2025-11-04

### Added - Process Management

**Background Job Execution** (Expression form):
- **SHELL "command" BACKGROUND INTO pid**: Launch command in background
  - `Then: job_id becomes SHELL "sleep 10" BACKGROUND INTO pid`
  - Returns process ID (PID) immediately without waiting
  - Stores PID in specified variable via INTO clause
  - Process runs independently of CNS script execution
  - Enables parallel execution of long-running tasks

**Process Control Operations** (Expression forms):
- **KILL pid [WITH signal]**: Send signal to process
  - `Then: result becomes KILL job_id` - Default SIGTERM (15)
  - `Then: result becomes KILL job_id WITH SIGKILL` - Force kill (signal 9)
  - `Then: result becomes KILL job_id WITH SIGTERM` - Graceful termination (15)
  - `Then: result becomes KILL job_id WITH SIGINT` - Interrupt (2)
  - `Then: result becomes KILL job_id WITH SIGHUP` - Hangup (1)
  - Returns TRUE on success, NIL on failure
  - SIGKILL (9) immediately removes process from tracking

- **WAIT FOR pid [WITH TIMEOUT seconds]**: Wait for process completion
  - `Then: exit_code becomes WAIT FOR job_id` - Wait indefinitely
  - `Then: exit_code becomes WAIT FOR job_id WITH TIMEOUT 10` - Wait with timeout
  - Returns exit code (0 for success) when process completes
  - Returns NIL if timeout expires before completion
  - Blocks execution until process finishes or timeout
  - Automatically removes completed processes from tracking

- **STATUS OF pid**: Check process status
  - `Then: status becomes STATUS OF job_id`
  - Returns "running" if process is active
  - Returns "completed" if process finished
  - Returns "not-found" if PID not tracked or already cleaned up
  - Non-blocking status check

### Use Cases
- **Parallel execution**: Run multiple tasks concurrently
- **Long-running jobs**: Background data processing, file operations
- **Graceful shutdown**: Terminate processes cleanly with signals
- **Timeout handling**: Detect and handle hung processes
- **Process monitoring**: Check status without blocking

### Examples
- **test-process-management.cns**: Comprehensive process management test
  - Launches background jobs with SHELL BACKGROUND
  - Waits for completion with WAIT FOR
  - Checks process status with STATUS OF
  - Tests timeout detection
  - Demonstrates parallel task execution

### Impact
- **Concurrency**: First-class support for parallel execution
- **Robustness**: Proper process lifecycle management
- **Flexibility**: Fine-grained control over background tasks
- **LLM-friendly**: Natural English syntax for all operations
- **100% backward compatible**: SHELL effect still works for foreground execution

### Technical Details
- Background processes tracked in global `*background-processes*` hash table
- Process management uses SBCL's `sb-ext:run-program` with `:wait nil`
- KILL uses Unix `/bin/kill` command for signal delivery
- WAIT FOR polls `sb-ext:process-exit-code` with 0.1s sleep intervals
- STATUS OF checks process state without blocking
- SHELL effect handler excludes BACKGROUND syntax to avoid conflicts
- All operations follow established `can-parse-X-p` / `try-X` pattern
- Operations added at src/cns.lisp:810-979
- Evaluation hooks at src/cns.lisp:4937-4956
- Process tracking ensures proper cleanup on completion

### Breaking Changes
None - All changes are additive and backward compatible.

---

## [1.10.0] - 2025-11-04

### Added - String Utilities

**String Operations** (Expression forms):
- **PAD text TO width [LEFT|RIGHT] [WITH char]**: Pad string to fixed width
  - `Then: padded becomes PAD name TO 10` - Right padding with spaces (default)
  - `Then: padded becomes PAD name TO 10 LEFT` - Left padding with spaces
  - `Then: padded becomes PAD number TO 5 WITH "0"` - Custom padding character (right)
  - `Then: padded becomes PAD number TO 5 LEFT WITH "0"` - Custom padding (left)
  - Default alignment: RIGHT (padding on left)
  - Default character: space
  - Returns original string if already at or exceeds target width
  
- **STRIP chars FROM text [LEFT|RIGHT]**: Remove characters from string
  - `Then: clean becomes STRIP " " FROM text` - Strip from both sides (default)
  - `Then: clean becomes STRIP " " FROM text LEFT` - Strip from left only
  - `Then: clean becomes STRIP " " FROM text RIGHT` - Strip from right only
  - `Then: clean becomes STRIP "!?" FROM text` - Strip multiple character types
  - Uses Common Lisp's string-trim/string-left-trim/string-right-trim
  
- **URL_ENCODE text**: URL percent-encoding
  - `Then: encoded becomes URL_ENCODE url`
  - Encodes special characters as %XX hex sequences
  - Preserves unreserved characters: A-Z, a-z, 0-9, -, _, ., ~
  - RFC 3986 compliant encoding
  
- **URL_DECODE text**: URL percent-decoding
  - `Then: decoded becomes URL_DECODE encoded`
  - Decodes %XX hex sequences back to characters
  - Converts + to space (query string compatible)
  - Handles malformed encoding gracefully

### Use Cases
- **PAD**: Formatting tables, aligning output, zero-padding numbers
- **STRIP**: Cleaning user input, removing whitespace, trimming punctuation
- **URL_ENCODE**: Building query strings, encoding parameters for HTTP requests
- **URL_DECODE**: Parsing URLs, decoding form data, handling encoded parameters

### Examples
- **test-string-utilities.cns**: Comprehensive test of all string utilities
  - Tests padding with different alignments and characters
  - Tests stripping from different sides with multiple character sets
  - Tests URL encoding/decoding and roundtrip conversions
  - Tests edge cases (no padding needed, no characters to strip)
  - Tests combined operations (chaining utilities together)

### Impact
- **Text formatting**: Professional output formatting with PAD
- **Data cleaning**: Robust input sanitization with STRIP
- **Web integration**: Native URL encoding/decoding for HTTP operations
- **LLM-friendly**: Natural English syntax for all operations
- **100% backward compatible**: No breaking changes

### Technical Details
- PAD supports LEFT/RIGHT alignment with custom padding characters
- STRIP supports LEFT/RIGHT/BOTH modes for flexible trimming
- URL encoding follows RFC 3986 unreserved character set
- All operations follow established `can-parse-X-p` / `try-X` pattern
- Integrated into expression evaluation (before comparison operators)
- Operations added at src/cns.lisp:647-804
- Evaluation hooks at src/cns.lisp:4743-4762

---

## [1.9.0] - 2025-11-04

### Added - Advanced Data Operations

**List Operations** (Expression forms):
- **REVERSE list**: Reverse order of list elements
  - `Then: reversed becomes REVERSE items`
  - Returns new list with elements in reverse order
- **UNIQUE list**: Remove duplicates from list
  - `Then: unique becomes UNIQUE items`
  - Preserves first occurrence of each element
- **SORT list**: Sort list of primitives (numbers or strings)
  - `Then: sorted becomes SORT items`
  - Numeric sort for numbers, alphabetical for strings
- **SORT list BY field**: Sort list of maps by field value
  - `Then: sorted becomes SORT items BY "field_name"`
  - Supports both numeric and string field values
- **SLICE list FROM start TO end**: Extract subset of list
  - `Then: subset becomes SLICE items FROM 0 TO 5`
  - Zero-based indexing, end index exclusive
  - Handles out-of-bounds gracefully

**Map Operations** (Expression forms):
- **KEYS OF map**: Extract all keys from map
  - `Then: all_keys becomes KEYS OF config`
  - Returns list of key strings
- **VALUES OF map**: Extract all values from map
  - `Then: all_values becomes VALUES OF config`
  - Returns list of values
- **MERGE map1 WITH map2**: Merge two maps
  - `Then: combined becomes MERGE defaults WITH overrides`
  - Second map overwrites conflicting keys
  - Returns new map without modifying originals

### Changed
- **Map initialization**: Maps declared with type `Map` now automatically initialize to empty hash table
  - Before: `config: Map` initialized to `nil`
  - After: `config: Map` initialized to empty hash table
  - Enables immediate use without explicit initialization

### Examples
- **test-list-operations.cns**: Comprehensive test of REVERSE, UNIQUE, SORT, SLICE
- **test-map-operations.cns**: Demonstrates KEYS OF, VALUES OF, MERGE
- **test-data-operations.cns**: Combined test of all v1.9.0 features

### Impact
- **Data processing**: Complete list manipulation without external libraries
- **Configuration management**: Easy map merging and inspection
- **LLM-friendly**: All operations use natural narrative syntax
- **100% backward compatible**: No breaking changes

### Technical Details
- All operations follow `can-parse-X-p` / `try-X` pattern for consistency
- Integrated into expression evaluation pipeline (before comparison operators)
- List operations use Common Lisp's built-in functions (reverse, sort, remove-duplicates, subseq)
- Map operations use hash table iteration (maphash) for efficiency

---

## [1.8.0] - 2025-11-04

### Added - CLI & File Operations
- **CLI Arguments**: Full command-line argument parsing
  - Positional arguments: `ARGS[0]`, `ARGS[1]`, `ARGS[n]`
  - Named arguments: `CLI ARG "name" WITH DEFAULT "value"`
  - Flag detection: `CLI HAS FLAG "name"`
  - Argument count: `CLI ARGS COUNT`
- **File System Operations**: Complete file management
  - File existence check: `FILE EXISTS path` (returns Boolean)
  - Directory listing: `LIST FILES IN path INTO var`
  - File deletion: `DELETE FILE path`
  - File renaming: `RENAME FILE old TO new`

### Changed - Major Refactoring
- **Expression parser overhaul**: Extracted all inline parsers to consistent `can-parse-X-p` / `try-X` pattern
- **File organization**: Moved utilities and globals to top, eliminated all SBCL compilation warnings
- **Effect system**: Extracted 15 effect handlers from monolithic `apply-effect` function (Nov 3)
- **Code consistency**: All parsers now follow unified architecture pattern

### Fixed
- **FILE EXISTS bug**: Paths with `/` (e.g., `/tmp/file.txt`) now parse correctly
  - Root cause: Division operator was intercepting paths before FILE EXISTS parser
  - Solution: Established explicit precedence hierarchy in expression evaluation
- **Legacy JSON parser**: Added clear deprecation notice and migration path to `parse-json-full`
- **Compilation warnings**: Zero SBCL warnings after file reorganization

### Documentation
- **New guides**: Comprehensive developer documentation added
  - `docs/development/GLOBAL-STATE.md` (350+ lines): All 15 global variables documented
  - `docs/development/ERROR-HANDLING.md` (550+ lines): Error handling patterns and best practices
  - Enhanced `REFACTORING-SESSION-2025-11-04.md`: Complete refactoring session notes
- **CLI examples**: 5 comprehensive test files demonstrating all argument types
- **File operation examples**: Complete test suite with all file system operations

### Quality Assurance
- **100% test pass rate**: All 38 tests passing (9 core, 24 features, 1 advanced, 4 LLM suite)
- Zero regressions from refactoring
- Zero compilation warnings
- All feature areas validated and working

### Developer Experience
- **Consistent architecture**: All parsers follow unified `can-parse-X-p` / `try-X` pattern
- **Clean codebase**: Utilities at top, zero forward reference warnings
- **Professional documentation**: 900+ lines of new developer guides
- **Easy onboarding**: Clear patterns for adding new features

### Impact
- **CLI tools ready**: Full command-line argument support enables production CLI applications
- **File operations complete**: Directory listing, file management fully implemented
- **Clean foundation**: Architecture ready for advanced features (process management, async operations)
- **100% backward compatibility**: No breaking changes

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
**Last Updated:** November 3, 2025
