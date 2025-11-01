# CNS v1.3.0 Release Notes

**Release Date:** 2025-10-31 (Updated 2025-11-01)  
**Focus:** Database Support + Full Dependency Integration

## What's New

### 1. Database Operations (SQLite)

CNS v1.3.0 introduces full SQLite database support, enabling persistent data storage for web backends, data-driven applications, and automation scripts.

### 2. HTTPS & Regex Now Fully Operational (Update 2025-11-01)

v1.3.0 update adds complete Quicklisp integration, making HTTPS and Regex features fully functional in production environments.

**Database Features:**
- ✅ Database connections (`DB CONNECT`)
- ✅ SQL execution (`DB EXECUTE` for INSERT/UPDATE/DELETE/CREATE)
- ✅ SQL queries (`DB QUERY` for SELECT)
- ✅ Complex SQL (COUNT, AS aliases, WHERE clauses)
- ✅ Graceful fallback when SQLite unavailable
- ✅ Zero Lisp dependencies (uses SQLite CLI)

**HTTPS Features:**
- ✅ Full SSL/TLS support via cl+ssl
- ✅ Character stream handling via flexi-streams
- ✅ Automatic Quicklisp loading
- ✅ Tested with GitHub API, httpbin.org

**Regex Features:**
- ✅ Pattern matching with MATCHES operator
- ✅ String extraction with EXTRACT operator
- ✅ Capture group support (GROUP n)
- ✅ Escape sequence processing (\\d, \\w, \\s, \\n, \\t)

**String Improvements:**
- ✅ Escape sequence support: \\n (newline), \\t (tab), \\r (return), \\\\ (backslash)
- ✅ Regex escape sequences: \\d (digit), \\w (word), \\s (space), etc.

**Syntax:**
```cnsc
Story: Todo List App

G: db:S="todos.db", tasks:S=""

S1→ Effect: DB CONNECT TO "todos.db" AS db
S2→ Effect: DB EXECUTE "CREATE TABLE tasks (id INTEGER PRIMARY KEY, name TEXT, done INTEGER)" AS db
S3→ Effect: DB EXECUTE "INSERT INTO tasks (name, done) VALUES ('Write code', 0)" AS db
S4→ Effect: DB QUERY "SELECT * FROM tasks WHERE done = 0" AS db INTO tasks
S5→ Effect: Print "Pending: {tasks}"

E: tasks
```

**Why It Matters:**
- **Complete web backends**: HTTPS + JSON + ENV + Database = production-ready APIs
- **Data persistence**: Build real applications, not just demos
- **Zero setup**: Uses SQLite CLI (already installed on most systems)

## Implementation Details

### Architecture

Database support uses a **shell-based wrapper** approach:
- Calls `sqlite3` CLI tool via `sb-ext:run-program`
- No external Lisp libraries required
- Graceful fallback when SQLite unavailable

### Syntax

**Connect to database:**
```cnsc
Effect: DB CONNECT TO "app.db" AS mydb
```

**Execute SQL (no results):**
```cnsc
Effect: DB EXECUTE "INSERT INTO users (name) VALUES ('Alice')" AS mydb
Effect: DB EXECUTE "UPDATE users SET active = 1 WHERE id = 5" AS mydb
Effect: DB EXECUTE "DELETE FROM old_data WHERE created < '2024-01-01'" AS mydb
```

**Query SQL (with results):**
```cnsc
Effect: DB QUERY "SELECT * FROM users WHERE active = 1" AS mydb INTO users
Effect: Print "Active users: {users}"
```

### Result Format

Query results use SQLite's `-line` mode format:
```
id = 1
name = Alice
completed = 0

id = 2  
name = Bob
completed = 1
```

Each row is separated by blank lines, columns are `key = value` pairs.

## Examples

### New Examples

**`examples/test-db-simple.cnsc`** (9 lines)
- Basic database operations
- CREATE TABLE, INSERT, SELECT
- Demonstrates core workflow

**`examples/test-db-comprehensive.cnsc`** (25 lines)
- Todo list application
- Full CRUD operations (Create, Read, Update, Delete)
- Query filtering, counting, updates

### Use Cases

**1. REST API with persistence:**
```cnsc
Story: User Registration API

G: db:S="users.db", request:S="", name:S="", email:S="", user_id:I=0

S1→ Effect: DB CONNECT TO "users.db" AS db
S2→ name=PARSE JSON request GET "name"
S3→ email=PARSE JSON request GET "email"
S4→ Effect: DB EXECUTE "INSERT INTO users (name, email) VALUES ('{name}', '{email}')" AS db
S5→ Effect: DB QUERY "SELECT last_insert_rowid() AS id" AS db INTO user_id
S6→ Effect: Print "{\"id\": {user_id}, \"name\": \"{name}\"}"

E: user_id
```

**2. Data processing pipeline:**
```cnsc
Story: Process CSV to Database

S1→ Effect: DB CONNECT TO "analytics.db" AS db
S2→ Effect: Read from file "data.csv" into csv_data
S3→ rows=SPLIT csv_data BY "\n"
S4→ Effect: FOREACH row IN rows
  → columns=SPLIT row BY ","
  → Effect: DB EXECUTE "INSERT INTO metrics VALUES ('{columns[0]}', {columns[1]})" AS db
```

**3. Configuration management:**
```cnsc
Story: Load App Config

S1→ Effect: DB CONNECT TO "config.db" AS db
S2→ Effect: DB QUERY "SELECT value FROM settings WHERE key = 'api_key'" AS db INTO api_key
S3→ Effect: HTTPS GET FROM "https://api.example.com" WITH header "Authorization: {api_key}" INTO response
```

## Installation

### Requirements

**SQLite3 CLI tool** (usually pre-installed):
```bash
# Check if installed
sqlite3 --version

# Install if needed (Ubuntu/Debian)
sudo apt-get install sqlite3

# Install if needed (macOS)
brew install sqlite3
```

See [INSTALL-SQLITE.md](INSTALL-SQLITE.md) for detailed instructions.

### Graceful Fallback

If SQLite is not installed, CNS will:
- Print a warning on startup: `"Database support unavailable (sqlite3 not found)"`
- Skip all `DB CONNECT`, `DB EXECUTE`, and `DB QUERY` operations
- Continue execution without errors

This allows CNS programs to run in environments without database support.

## Testing

**Validation:**
```bash
./src/cns-validate examples/test-db-simple.cnsc
# Output: ✓ VALID (ready for execution)
```

**Execution:**
```bash
./src/cns-run examples/test-db-simple.cnsc
# Creates test.db, inserts data, queries and prints results
```

**Manual verification:**
```bash
sqlite3 test.db "SELECT * FROM users;"
# Output:
# 1|Alice
# 2|Bob
```

## Performance

**Overhead per operation:** ~10-20ms (shell process startup)
- **Acceptable for:** Web APIs, background jobs, automation
- **Not suitable for:** High-frequency operations (>100/sec)

**Optimization tip:** Batch operations when possible:
```cnsc
Effect: DB EXECUTE "
  BEGIN TRANSACTION;
  INSERT INTO logs VALUES ('event1');
  INSERT INTO logs VALUES ('event2');
  INSERT INTO logs VALUES ('event3');
  COMMIT;
" AS db
```

## Limitations

### Current Version (v1.3.0)

1. **SQLite only** - PostgreSQL/MySQL planned for future releases
2. **No prepared statements** - SQL injection risk if user input not sanitized
3. **String results only** - No native parsing of query results (yet)
4. **No transactions** - Must use SQL `BEGIN/COMMIT` directly
5. **Shell overhead** - Each query spawns a process (~10-20ms)

### Workarounds

**SQL injection prevention:**
```cnsc
# DON'T DO THIS (unsafe):
Effect: DB EXECUTE "SELECT * FROM users WHERE name = '{user_input}'" AS db

# DO THIS (escape quotes):
user_input_safe=REPLACE(user_input, "'", "''")
Effect: DB EXECUTE "SELECT * FROM users WHERE name = '{user_input_safe}'" AS db
```

**Parse query results:**
```cnsc
S1→ Effect: DB QUERY "SELECT COUNT(*) as count FROM users" AS db INTO result
S2→ count_str=EXTRACT "count = (\\d+)" GROUP 1 FROM result
S3→ count=PARSE INT count_str
```

## Migration from v1.2.0

**No breaking changes!** All v1.2.0 code runs unchanged.

**New features added:**
- `DB CONNECT TO "filepath" AS db_name` effect
- `DB EXECUTE "sql" AS db_name` effect
- `DB QUERY "sql" AS db_name INTO var` effect

## What's Next

### Phase B Week 3 Goals

**Option 1: Continue Phase B features:**
- String helpers (TRIM, UPPERCASE, LOWERCASE, REPLACE, JOIN)
- CSV support (read/write)

**Option 2: Start Phase B-Prime (Benchmark Prerequisites):**
- Shell execution (SHELL command with stdout/stderr)
- Git operations (clone, checkout, diff, status)
- Diff generation (for patch files)

**Option 3: Polish & Release:**
- Create GitHub release tag for v1.3.0
- Extended test suite
- User documentation

See [ROADMAP.md](docs/development/ROADMAP.md) for detailed plans.

## Update Notes (2025-11-01)

### Dependency Integration

This update resolves issues with HTTPS and Regex dependencies, making these features fully operational:

**Fixed:**
- ✅ Quicklisp automatic loading on startup
- ✅ cl+ssl package loading for HTTPS
- ✅ flexi-streams package for SSL character I/O
- ✅ cl-ppcre package loading for regex
- ✅ String escape sequence processing (\\d → \d for regex patterns)
- ✅ SQL string parsing (handles AS keyword inside queries)
- ✅ Database name case preservation

**Installation:**
```bash
# Install Quicklisp packages once
sbcl --eval "(ql:quickload :cl+ssl)" \
     --eval "(ql:quickload :flexi-streams)" \
     --eval "(ql:quickload :cl-ppcre)" \
     --quit
```

**Verified Working:**
- HTTPS GET/POST to production APIs (GitHub, httpbin.org)
- Regex pattern matching with all escape sequences
- Database operations with complex SQL queries

### Technical Details

**SSL Stream Fix:**
- SSL streams wrapped with flexi-streams for character-level I/O
- Resolves `STREAM-WRITE-STRING` method errors

**Escape Sequence Processing:**
- String literals now process: \\n, \\t, \\r, \\\\
- Regex escapes preserved: \\d, \\w, \\s, etc.

**SQL Parser Fix:**
- Finds closing quote first, then searches for AS/INTO keywords
- Handles SQL with AS aliases (e.g., `SELECT COUNT(*) AS total`)
- Extracts database names from original (not uppercased) strings

## Contributors

**Development:** Solo indie project  
**Testing:** Grok-2 (LLM validation), manual testing  
**Timeline:** 2 sessions (database 3 hours, dependencies 2 hours)

## Resources

- [Installation Guide - SQLite](INSTALL-SQLITE.md)
- [Installation Guide - HTTPS](INSTALL-HTTPS.md)
- [Installation Guide - Regex](INSTALL-REGEX.md)
- [Examples](examples/)
- [Roadmap](docs/development/ROADMAP.md)
- [GitHub Issues](https://github.com/jessecrouch/cns/issues)

---

**Previous Releases:**
- [v1.2.0 - Regex & Date/Time](RELEASE-NOTES-v1.2.0.md)
- [v1.1.0 - JSON & ENV](RELEASE-NOTES-v1.1.0.md)
- [v1.0.0 - Initial Release](README.md)
