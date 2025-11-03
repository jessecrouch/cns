# CNS Examples - Pattern Guide for LLMs

**Purpose:** This directory teaches CNS patterns through curated examples organized by complexity.

**Philosophy:** Learn by example, not by documentation. Every file demonstrates a working pattern.

---

## 🎯 Learning Path

```
1. Start with core/        - Master fundamental patterns
2. Explore features/       - Learn one feature at a time  
3. Study advanced/         - See real-world applications
```

**Recommendation for LLMs:** Read `core/` examples first to learn CNS syntax patterns. Then reference `features/` for specific capabilities.

---

## 📁 Directory Structure

### core/ (11 files)
**Canonical examples demonstrating fundamental CNS patterns**

Essential learning examples:
- `hello.cns` / `hello.cnsc` - Minimal CNS program
- `fibonacci.cns` / `fibonacci.cnsc` - Loop with multiple state variables
- `factorial.cns` - Basic loop and accumulation
- `is-prime.cns` / `is-prime.cnsc` - Boolean logic and conditionals
- `gcd.cns` - Classical algorithm (Euclidean)
- `collatz.cns` - Complex conditionals
- `sum-range.cns` - Range iteration
- `power.cns` - Repeated multiplication

**Pattern Coverage:**
- Variable declaration
- Basic arithmetic
- If/Otherwise conditionals
- Loop control (repeat from)
- State accumulation
- Boolean logic

### features/ (25 files)
**One example per major CNS feature**

Organized by feature domain:

**HTTP & APIs:**
- `api-demo.cns` / `api-demo.cnsc` - REST API calls
- `test-http-get.cns` - GET requests
- `test-http-post.cns` - POST with data
- `test-https.cns` / `test-https.cnsc` - HTTPS connections

**Data Parsing:**
- `test-json-nested.cns` / `test-json-nested.cnsc` - JSON with dot notation
- `test-json-comprehensive.cns` - All JSON features
- `test-csv.cns` - CSV read/write with headers
- `test-env-vars.cns` / `test-env-vars.cnsc` - Environment variables

**Text Processing:**
- `test-regex.cns` - Pattern matching with MATCHES/EXTRACT
- `test-string-helpers.cns` / `test-string-helpers.cnsc` - TRIM, UPPERCASE, etc.

**Date & Time:**
- `test-datetime.cns` - NOW, TIMESTAMP, FORMAT TIME, arithmetic

**Database:**
- `test-db-simple.cnsc` - Basic SQLite operations
- `test-db-comprehensive.cnsc` - Full CRUD workflow

**System Integration:**
- `test-shell.cns` - Shell command execution
- `test-git-workflow.cns` - Complete git workflow
- `test-find-basic.cns` - File discovery (FIND)
- `test-grep-basic.cns` - Content search (GREP)

**Core Features:**
- `test-lists.cns` - List operations
- `error-handling-demo.cns` - Error section usage
- `file-demo.cns` - File I/O (read, write, append)

**Pattern Coverage:**
- HTTP GET/POST with status codes
- JSON parsing (nested, arrays, dot notation)
- CSV headers and data manipulation
- Regex pattern matching
- String manipulation helpers
- Date/time arithmetic
- Database CRUD
- Shell execution with I/O capture
- Git operations
- File/content search
- List operations
- Error handling

### advanced/ (6 files)
**Complex multi-feature applications**

Real-world examples:
- `killer-app-demo.cns` - Multi-API orchestration
- `language-detector.cns` - Complex text analysis
- `todo-api.cns` - REST API server
- `text-processor-functions.cns` - Functions with recursion
- `advanced-webserver.cns` - TCP socket server
- `automation-agent.cns` - Automated code agent (FIND/GREP/GIT)

**Pattern Coverage:**
- Multi-step workflows
- Function definitions and calls
- Complex control flow
- Real TCP sockets
- Multi-API coordination
- Code navigation patterns

---

## 🔑 Key CNS Patterns (For LLM Learning)

### Pattern 1: Variable Declaration
```cns
Given:
  count: Integer = 0
  name: String = "default"
  items: List = []
  data: Map = {}
```

**Common Mistake:**
```cns
# ❌ Wrong - missing type
Given:
  count = 0
  
# ✅ Right
Given:
  count: Integer = 0
```

### Pattern 2: State Transformation
```cns
Step 1 → Update counter
  Then: count becomes count + 1
  Then: total becomes total + count
```

**Common Mistake:**
```cns
# ❌ Wrong - "Set" syntax doesn't work everywhere
Step 1:
  Set count to count + 1
  
# ✅ Right - Use "becomes"
Step 1:
  Then: count becomes count + 1
```

### Pattern 3: Conditionals
```cns
Step 2 → Check condition
  If: count > 10
    Then: status becomes "high"
  Otherwise:
    Then: status becomes "low"
```

**Important:** Control flow (`repeat from`, `go to`) ONLY works inside If/Otherwise!

```cns
# ❌ Wrong - control flow outside conditional
Step 3:
  Then: repeat from Step 1

# ✅ Right - control flow in conditional
Step 3:
  If: count < 100
    Then: repeat from Step 1
  Otherwise: go to End
```

### Pattern 4: Loops
```cns
Story: Count to 10

Given:
  counter: Integer = 1

Step 1 → Increment
  Then: counter becomes counter + 1

Step 2 → Check if done
  If: counter < 10
    Then: repeat from Step 1

End: Return counter
```

**Pattern:** Separate increment and check steps for clarity

### Pattern 5: HTTP Requests
```cns
Given:
  url: String = "https://api.example.com/data"
  response: String = ""
  status: Integer = 0

Step 1 → Call API
  Effect: HTTP GET from url into response with status code status
  
Step 2 → Check success
  If: status == 200
    Then: data becomes PARSE JSON response GET "result"
```

**Key:** Use `with status code variable` to capture HTTP status

### Pattern 6: JSON Parsing
```cns
# Nested objects with dot notation
Then: name becomes PARSE JSON response GET "user.profile.name"

# Array indexing
Then: first becomes PARSE JSON response GET "items[0]"

# Mixed
Then: email becomes PARSE JSON response GET "users[2].email"

# Array length
Then: count becomes PARSE JSON response GET "items" LENGTH
```

### Pattern 7: Expression Limitations (IMPORTANT!)

**What DOESN'T work:**
```cns
# ❌ Literal-first multiplication
Then: result becomes 3 * n

# ❌ Multi-operator expressions
Then: result becomes a + b * c
```

**Workarounds that DO work:**
```cns
# ✅ Variable-first multiplication
Then: result becomes n * 3

# ✅ Split multi-operator into steps
Then: temp becomes b * c
Then: result becomes a + temp
```

**Why:** CNS expression parser is conservative. LLMs should generate simple expressions.

### Pattern 8: String Concatenation
```cns
# Multi-part concatenation with +
Then: message becomes "Count: " + count + " items"

# Works in PRINT
Effect: Print "Found " + num_files + " files"
```

### Pattern 9: FIND/GREP (Code Navigation)
```cns
# Find files by pattern
FIND "*.cns" IN "examples" INTO cns_files WITH COUNT count

# Search file contents
GREP "function" IN "src/code.lisp" INTO matches

# Search multiple files (accepts list variable)
FIND "*.lisp" IN "src" INTO files
GREP "TODO" IN files INTO todos WITH COUNT num_todos
```

**Match data structure:**
```cns
# Each GREP match is [file, line_number, text]
FOREACH match IN matches:
  SET file = match[0]
  SET line_num = match[1]
  SET text = match[2]
  PRINT file + ":" + line_num + " - " + text
END
```

### Pattern 10: CNSC Compact Format

**Verbose CNS:**
```cns
Story: Calculate factorial

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply
  Then: result becomes result * n
  Then: n becomes n - 1

Step 2 → Check done
  If: n > 1
    Then: repeat from Step 1

End: Return result
```

**CNSC (62% smaller):**
```cnsc
Story: Calculate factorial

G: n:I=5, result:I=1

S1→ result=result*n
  → n=n-1

S2→ n>1? ->S1

E: result
```

**When to use CNSC:** Production code, LLM context optimization, large programs

---

## 🚀 Running Examples

### From Repository Root
```bash
# Run verbose format
./cns-run examples/core/factorial.cns

# Run compact format (auto-expands)
./cns-run examples/core/fibonacci.cnsc

# Run feature example
./cns-run examples/features/test-http-get.cns

# Run advanced example
./cns-run examples/advanced/killer-app-demo.cns
```

### Test All Examples
```bash
# From repository root
./test-all-examples.sh
```

**Expected:** 50%+ pass rate (some examples require optional dependencies)

---

## 📚 Common Patterns by Use Case

### Use Case: REST API Client
**See:** `features/api-demo.cns`

**Pattern:**
1. HTTP GET to fetch data
2. Parse JSON response
3. Extract specific fields with dot notation
4. Handle errors with status codes

### Use Case: File Processing
**See:** `features/file-demo.cns`

**Pattern:**
1. READ FILE into variable
2. Process content (split, parse, etc.)
3. WRITE FILE or APPEND FILE
4. Handle file not found errors

### Use Case: Database Application
**See:** `features/test-db-comprehensive.cnsc`

**Pattern:**
1. CONNECT to database
2. EXECUTE DDL (CREATE TABLE)
3. EXECUTE DML (INSERT, UPDATE)
4. QUERY SELECT into variable
5. Parse results

### Use Case: Git Workflow
**See:** `features/test-git-workflow.cns`

**Pattern:**
1. GIT STATUS to check state
2. GIT ADD files
3. GIT COMMIT with message
4. GIT DIFF to generate patch

### Use Case: Code Automation Agent
**See:** `advanced/automation-agent.cns`

**Pattern:**
1. FIND files by pattern
2. GREP for specific content
3. Analyze matches
4. GIT operations for changes

---

## ⚠️ Common LLM Mistakes

### Mistake 1: Forgetting Type Annotations
```cns
# ❌ Wrong
Given:
  count = 0

# ✅ Right
Given:
  count: Integer = 0
```

### Mistake 2: Using "Set" in Then Clauses
```cns
# ❌ Wrong - "Set" is for standalone actions
Step 1:
  Then: Set count to 5

# ✅ Right - "becomes" in Then clauses
Step 1:
  Then: count becomes 5
```

### Mistake 3: Control Flow Outside Conditionals
```cns
# ❌ Wrong - silently ignored
Step 1:
  Then: count becomes count + 1
  Then: repeat from Step 1

# ✅ Right - inside If/Otherwise
Step 1:
  Then: count becomes count + 1

Step 2:
  If: count < 10
    Then: repeat from Step 1
```

### Mistake 4: Complex Expressions
```cns
# ❌ Wrong - multi-operator fails
Then: result becomes 3 * n + 1

# ✅ Right - split into steps
Then: temp becomes n * 3
Then: result becomes temp + 1
```

### Mistake 5: Missing WITH Clauses
```cns
# ❌ Wrong - can't capture status
HTTP GET from url into response

# ✅ Right - capture status for error handling
HTTP GET from url into response with status code status
```

---

## 🎓 Learning Recommendations

### For LLMs Generating CNS Code:

1. **Start with core/ examples** - Learn fundamental patterns
2. **Keep expressions simple** - Avoid multi-operator, literal-first
3. **Always use type annotations** - In Given section
4. **Control flow in conditionals** - repeat/go to only in If/Otherwise
5. **Capture status codes** - Always use WITH clauses for HTTP, Shell, Git
6. **Split complex logic** - One operation per Then clause when possible
7. **Use CNSC for production** - 62% smaller, better context efficiency

### For Humans Learning CNS:

1. **Read core/ examples** - Understand narrative structure
2. **Run with verbose output** - See execution traces
3. **Study features/ one at a time** - Master one feature before next
4. **Experiment with advanced/** - See real-world patterns
5. **Check error messages** - They include working examples
6. **Use validator** - `./cns-validate yourfile.cns` before running

---

## 📊 Example Statistics

**Total Examples:** 42 files (92 before cleanup - 54% reduction)

**By Format:**
- CNS (verbose): 32 files
- CNSC (compact): 10 files

**By Category:**
- Core: 11 files (26%)
- Features: 25 files (60%)
- Advanced: 6 files (14%)

**Coverage:**
- HTTP/HTTPS: ✅
- JSON parsing: ✅
- Database (SQLite): ✅
- File I/O: ✅
- Git operations: ✅
- Shell execution: ✅
- Find/Grep: ✅
- Regex: ✅
- Date/Time: ✅
- CSV: ✅
- Error handling: ✅
- Functions: ✅
- TCP Sockets: ✅

---

## 🔗 Additional Resources

**Language Documentation:**
- [QUICKSTART.md](../QUICKSTART.md) - 5-minute tutorial
- [SYNTAX.md](../SYNTAX.md) - Complete language reference (includes functions, CLI args, all features)
- [CNSC Guide](../docs/guides/CNSC-COMPACT.md) - Compact format reference
- [CLI Arguments](../docs/guides/CLI-ARGUMENTS.md) - Command-line arguments (v1.8.0)
- [CHANGELOG.md](../CHANGELOG.md) - Feature history

**Installation:**
- [Install HTTPS](../docs/install/INSTALL-HTTPS.md) - SSL/TLS support
- [Install Regex](../docs/install/INSTALL-REGEX.md) - cl-ppcre setup
- [Install SQLite](../docs/install/INSTALL-SQLITE.md) - Database support

**Development:**
- [ROADMAP.md](../docs/development/ROADMAP.md) - Future plans
- [Lisp Debugging](../docs/development/LISP-DEBUGGING-GUIDE.md) - Internals

---

## 💡 Philosophy

**Every example teaches a pattern.**

CNS is optimized for LLM code generation. These examples demonstrate:
- Clear narrative structure
- Explicit state transformations
- Predictable control flow
- Simple expression patterns
- Comprehensive feature coverage

**Goal:** LLMs should achieve 80%+ first-try success rate by learning from these patterns.

---

**Last Updated:** November 2, 2025  
**Examples Version:** v1.7.0  
**Maintained by:** CNS Development Team
