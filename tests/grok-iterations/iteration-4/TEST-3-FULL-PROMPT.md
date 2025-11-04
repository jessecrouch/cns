# Test 3: Task Runner REST API

## Task

Build a REST API server in CNS that accepts task submissions, runs them as background jobs, stores results in a database, and provides endpoints to check task status.

## Endpoints

1. **POST /tasks** - Submit a new task (command in JSON body)
2. **GET /tasks/<id>** - Get task status by ID
3. **GET /tasks** - List all tasks
4. **DELETE /tasks/<id>** - Kill a running task

## Database Schema

```sql
CREATE TABLE IF NOT EXISTS tasks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  command TEXT NOT NULL,
  pid INTEGER,
  status TEXT NOT NULL,
  exit_code INTEGER
)
```

## Example Usage

```bash
# Submit a task
curl -X POST http://localhost:8080/tasks -d '{"command":"sleep 10"}'
# Response: {"task_id": 1, "status": "running", "pid": 12345}

# Check task status
curl http://localhost:8080/tasks/1
# Response: {"task_id": 1, "status": "running", "command": "sleep 10", "pid": 12345}

# Kill task
curl -X DELETE http://localhost:8080/tasks/1
# Response: {"task_id": 1, "status": "killed"}
```

---

**Generate a complete CNS program that solves this task using the syntax reference below.**

# CNS Complete Reference for LLMs

Task: {TASK}

Generate a complete CNS program that solves this task.

---

## QUICK REFERENCE: Function Lookup Table

| Need to... | Use This ✅ | NOT This ❌ |
|------------|-------------|-------------|
| **Time** |
| Get current time | `TIMESTAMP()` | `NOW()`, `TIME()`, `CURRENT_TIME()` |
| **Strings** |
| To uppercase | `UPPERCASE text` | `UPPER(text)`, `text.upper()` |
| To lowercase | `LOWERCASE text` | `LOWER(text)`, `text.lower()` |
| Trim whitespace | `TRIM text` | `STRIP(text)`, `text.trim()` |
| Pad string | `PAD text TO 10` | `text.ljust()`, `PAD(text, 10)` |
| Pad with char | `PAD text TO 10 WITH "0"` | `text.zfill()` |
| Strip characters | `STRIP " " FROM text` | `text.strip()`, `STRIP(text)` |
| URL encode | `URL_ENCODE text` | `urllib.quote()`, `encodeURI()` |
| URL decode | `URL_DECODE text` | `urllib.unquote()`, `decodeURI()` |
| Split string | `SPLIT text BY "\n"` | `SPLIT(text, "\n")`, `text.split()` |
| Join list | `JOIN items WITH ","` | `JOIN(items, ",")`, `",".join()` |
| Replace text | `REPLACE "old" WITH "new" IN text` | `REPLACE(text, "old", "new")` |
| Parse to integer | `PARSE_INT text` | `INT(text)`, `parseInt()` |
| Parse to float | `PARSE_FLOAT text` | `FLOAT(text)`, `parseFloat()` |
| **Lists** |
| Get length | `LENGTH OF items` | `LEN(items)`, `items.length`, `SIZE(items)` |
| Get first item | `FIRST FROM items` | `items[0]`, `items at 0`, `HEAD(items)` |
| Add to list | `Effect: ADD item TO LIST items` | `items.append()`, `items.push()` |
| Reverse list | `REVERSE items` | `items.reverse()`, `REVERSE(items)` |
| Remove duplicates | `UNIQUE items` | `UNIQUE(items)`, `set(items)` |
| Sort list | `SORT items` | `SORT(items)`, `items.sort()` |
| Sort by field | `SORT items BY "field"` | `sortBy()`, `ORDER BY` |
| Slice list | `SLICE items FROM 0 TO 5` | `items[0:5]`, `SLICE(items, 0, 5)` |
| **Maps/Headers** |
| Get all keys | `KEYS OF config` | `config.keys()`, `KEYS(config)` |
| Get all values | `VALUES OF config` | `config.values()`, `VALUES(config)` |
| Merge maps | `MERGE map1 WITH map2` | `{...map1, ...map2}`, `MERGE(map1, map2)` |
| Access map value | ❌ Not available | `map["key"]`, `map.get("key")`, `REQUEST_HEADERS["X-Real-IP"]` |
| **JSON** |
| Parse JSON | `PARSE JSON response GET "key"` | `JSON.parse()`, `PARSE(response)` |
| Nested field | `PARSE JSON data GET "user.name"` | `data["user"]["name"]` |
| **Files** |
| Read file | `READ FROM FILE "path"` or `READ FROM FILE variable` | `READ("path")`, `READFILE("path")` |
| Write file | `Effect: WRITE content TO FILE "path"` | `WRITE("data", "path")` |
| Append file | `Effect: APPEND content TO FILE "path"` | `APPEND("data", "path")` |
| **HTTP** |
| GET request | `Effect: HTTP GET from url into response` | `GET(url)`, `HTTP.GET()` |
| POST request | `Effect: HTTP POST to url with body data into response` | `POST(url, data)` |
| With status | `Effect: HTTP GET from url into response with status code status` | - |
| **Network (Server)** |
| Create socket | `Effect: Create socket server_socket on port` | `SOCKET(port)` |
| Accept connection | `Effect: Accept connection on server_socket` | `ACCEPT(socket)` |
| Read request | `Effect: Network read` | `READ()`, `RECV()` |
| Send response | `Effect: Send response to client` | `SEND(response)` |
| Close connection | `Effect: Close connection client_socket` | `CLOSE(socket)` |
| **CLI Arguments** |
| Positional arg | `ARGS[0]`, `ARGS[1]` | `sys.argv[1]`, `$1` |
| Named argument | `ARG("--port", "8080")` | `argparse`, `getopts` |
| Boolean flag | `HAS_FLAG("--verbose")` | `--verbose` check |
| **Environment/System** |
| Get env var | `ENV("VAR_NAME", "default")` | `os.getenv()`, `$VAR` |
| Run shell | `Effect: SHELL "command" INTO output` | `system()`, `exec()` |
| **Process Management** |
| Background job | `pid becomes SHELL "cmd" BACKGROUND INTO job` | `subprocess.Popen()`, `&` |
| Kill process | `result becomes KILL pid` | `kill()`, `pkill` |
| Kill with signal | `result becomes KILL pid WITH SIGKILL` | `kill -9` |
| Wait for process | `exit_code becomes WAIT FOR pid` | `wait()`, `waitpid()` |
| Wait with timeout | `exit_code becomes WAIT FOR pid WITH TIMEOUT 10` | - |
| Check process status | `status becomes STATUS OF pid` | `ps`, `pgrep` |
| **Comparisons** |
| Equals | `If: x = 5` | `If: x == 5` |
| Not equals | `If: NOT (x = 5)` | `If: x != 5`, `If: x <> 5` |
| Contains | ❌ Use exact `=` | `CONTAINS`, `IN` |
| **Math** |
| Square root | `SQRT OF x` | `SQRT(x)`, `Math.sqrt()`, `SQRT x` |
| Power | `POW x TO y` | `POW(x, y)`, `x ** y`, `POWER()` |
| Absolute value | `ABS OF x` | `ABS(x)`, `Math.abs()` |
| Round | `ROUND x` | `ROUND(x)`, `Math.round()` |
| Floor | `FLOOR x` | `FLOOR(x)`, `Math.floor()` |
| Ceiling | `CEIL x` | `CEIL(x)`, `Math.ceil()`, `CEILING()` |
| Minimum | `MIN OF a AND b` | `MIN(a, b)`, `Math.min()` |
| Maximum | `MAX OF a AND b` | `MAX(a, b)`, `Math.max()` |
| Random 0-1 | `RANDOM` | `RANDOM()`, `Math.random()` |
| Random range | `RANDOM FROM 1 TO 10` | `RANDOM(1, 10)`, `randint()` |
| Modulo | `x % y` | `MOD(x, y)` |
| **Functions** |
| Define function | `Story: FuncName (function)` | `def func():`, `function func()` |
| Call function | `result becomes FuncName(arg1, arg2)` | `func(arg1, arg2)` |

---

## VALIDATION CHECKLIST

Before submitting your CNS code, verify:

**Structure:**
- [ ] `Story:` line exists with program name
- [ ] `Given:` section declares ALL variables before use
- [ ] Every step has `→` arrow and description
- [ ] Every step has `Because:` clause explaining why
- [ ] `End: Return value` section exists

**Syntax:**
- [ ] Comparisons use `=` not `==`
- [ ] Booleans support `TRUE`/`FALSE`, `True`/`False`, or `true`/`false` (all work)
- [ ] All control flow (`go to`, `repeat from`) inside If/Otherwise blocks
- [ ] No literal-first expressions (`n * 3` not `3 * n`)
- [ ] No multi-operator math (`x * 3 + 1` split into two lines)
- [ ] No parentheses in expressions (use temp variables)
- [ ] No functions from other languages (`NOW()`, `SPLIT()`, `ENV()`)

**Variables:**
- [ ] All variables declared in `Given:` section
- [ ] Types specified: Integer, String, List, Map, Socket, Boolean
- [ ] Auto-populated variables NOT declared (REQUEST_METHOD, REQUEST_PATH, etc.)
- [ ] CLI args accessed via ARGS[], ARG(), HAS_FLAG() - not declared as variables
- [ ] No array indexing syntax (`arr[0]` or `arr at 0`)

**Effects:**
- [ ] Print uses string interpolation: `"Count: {count}"`
- [ ] File paths are LITERAL STRINGS in quotes: `"/tmp/file.txt"` (not variables!)
- [ ] HTTP effects use full syntax: `HTTP GET from url into response`
- [ ] Network effects in correct order: Create → Accept → Read → Send → Close
- [ ] No attempts to access REQUEST_HEADERS individual values

---

## Structure Requirements

### Minimal Program
```cns
Story: Program Name

Given:
  variable: Type = initial_value

Step 1 → Description
  Because: Reason for this step
  Then: variable becomes expression
  Effect: Print "Result: {variable}"

End: Return variable
```

### With Control Flow
```cns
Story: Loop Example

Given:
  count: Integer = 0
  limit: Integer = 10

Step 1 → Increment counter
  Because: Count up to limit
  Then: count becomes count + 1

Step 2 → Check if done
  Because: Stop at limit
  If: count < limit
    Then: repeat from Step 1
  Otherwise:
    Then: go to End

End: Return count
```

---

## Variable Declarations

```cns
Given:
  count: Integer = 0
  name: String = "default"
  items: List = []
  data: Map = {}
  server: Socket
  flag: Boolean = TRUE
```

**Types**: Integer, String, List, Map, Socket, Boolean

**Boolean values**: `TRUE`/`FALSE` (recommended), `True`/`False`, or `true`/`false` (all supported for LLM compatibility)

**Rules:**
- ALL variables must be declared before use
- Initial values required (except Socket)
- Use `TRUE`/`FALSE` for booleans (uppercase!)

---

## Built-in Variables (DO NOT DECLARE)

After `Effect: Network read`, these are automatically populated:

| Variable | Contains | Example | Usable? |
|----------|----------|---------|---------|
| `REQUEST_METHOD` | HTTP method | `"GET"`, `"POST"` | ✅ Yes |
| `REQUEST_PATH` | URL path | `"/"`, `"/api/users"` | ✅ Yes |
| `REQUEST_BODY` | POST data | `"{\"name\":\"Alice\"}"` | ✅ Yes |
| `REQUEST_QUERY` | Query string | `"?id=123"` | ✅ Yes |
| `REQUEST_HEADERS` | Header map | Map object | ❌ No - Cannot access individual headers |

**Usage:**
```cns
Step 1 → Read HTTP request
  Effect: Network read
  Then: method becomes REQUEST_METHOD  # Don't declare REQUEST_METHOD!
  Then: path becomes REQUEST_PATH
```

**CRITICAL**: 
- Do NOT declare these in `Given:` section!
- `REQUEST_HEADERS` exists but individual headers cannot be accessed (no map["key"] syntax)
- If you need client IP or specific headers, use a placeholder value or omit that feature

---

## Expressions

### ✅ SAFE (Always Works)

```cns
# Arithmetic
Then: result becomes x + 5        # Variable-first
Then: sum becomes a + b           # Simple binary operation
Then: product becomes x * y       # Multiplication
Then: quotient becomes x / 2      # Division
Then: remainder becomes x % 3     # Modulo

# Math functions
Then: root becomes SQRT OF 16     # Square root (result: 4.0)
Then: power becomes POW 2 TO 3    # Exponentiation (result: 8)
Then: distance becomes ABS OF -42 # Absolute value (result: 42)
Then: rounded becomes ROUND 3.7   # Round to nearest (result: 4)
Then: floored becomes FLOOR 3.9   # Round down (result: 3)
Then: ceiled becomes CEIL 3.1     # Round up (result: 4)
Then: lowest becomes MIN OF 10 AND 20   # Minimum (result: 10)
Then: highest becomes MAX OF 10 AND 20  # Maximum (result: 20)
Then: chance becomes RANDOM       # Random 0.0-1.0 (e.g., 0.629)
Then: dice becomes RANDOM FROM 1 TO 6   # Random integer (e.g., 4)
```

**Note on floats**: CNS now supports decimal literals: `3.14`, `-2.5`, `98.6`

### ✅ BOTH ORDERS WORK! (Literal-first bug FIXED)

```cns
Then: result becomes 3 * n        # ✅ Works! (15)
Then: result becomes n * 3        # ✅ Works! (15)
Then: result becomes 10 - x       # ✅ Works! (5)
Then: result becomes x - 1        # ✅ Works! (4)
Then: result becomes 100 / n      # ✅ Works! (20)
```

**NEW in v1.1:** Literal-first expressions now work correctly! No need to rewrite `3 * n` as `n * 3`.

### ⚠️ STILL LIMITATIONS (Multi-operator, Parentheses)

```cns
Then: result becomes n * 3 + 1    # ❌ Multi-operator → NIL
Then: avg becomes (a + b) / 2     # ❌ Parentheses → NIL
Then: result becomes 2 + 3 * 4    # ⚠️ Returns 20, not 14 (left-to-right evaluation)
Then: x becomes SQRT 16           # ❌ Missing "OF" → NIL (use "SQRT OF 16")
Then: y becomes POW 2 3           # ❌ Missing "TO" → NIL (use "POW 2 TO 3")
```

### ✅ WORKAROUNDS

```cns
# Multi-operator expressions - split into steps:
Then: temp becomes n * 3
Then: result becomes temp + 1     # Split into steps

# Parentheses - use temporary variable:
Then: sum becomes a + b
Then: avg becomes sum / 2         # Use temporary variable

# Control operator precedence:
Then: product becomes b * c
Then: result becomes a + product  # Now: a + (b*c)
```

**KEY RULE**: CNS evaluates left-to-right with NO operator precedence!
- Math: `2 + 3 * 4 = 14` (multiply first)
- CNS: `2 + 3 * 4 = 20` (left-to-right: (2+3)*4)

---

## Comparison Operators

```cns
If: x = 5           # Equal (single = recommended)
If: x == 5          # Equal (double == also supported for LLM compatibility)
If: x > 10          # Greater than
If: x < 100         # Less than
If: x >= 50         # Greater or equal
If: x <= 200        # Less or equal
If: x != 5          # Not equal (supported for LLM compatibility)
If: NOT (x = 5)     # Not equal (alternative syntax)
```

**BOTH WORK**: Use `=` or `==` for equality (both supported)
**BOTH WORK**: Use `!=` or `NOT (x = y)` for inequality (both supported)

---

## Control Flow

### If/Otherwise

```cns
Step 1 → Check value
  Because: Branch based on condition
  If: x > 10
    Then: status becomes "high"
  Otherwise:
    Then: status becomes "low"
```

### Loops (repeat from)

```cns
Step 1 → Increment
  Because: Count upward
  Then: count becomes count + 1

Step 2 → Check if done
  Because: Stop at limit
  If: count < 10
    Then: repeat from Step 1
  Otherwise:
    Then: go to End
```

### Jump to Step

```cns
Step 1 → Check condition
  Because: Route to appropriate handler
  If: path = "/"
    Then: go to Step 5
  Otherwise:
    Then: go to Step 10
```

**CRITICAL RULE**: Control flow (`go to`, `repeat from`) MUST be inside If/Otherwise blocks!

❌ **This is IGNORED:**
```cns
Step 1:
  Then: count becomes count + 1
  Then: repeat from Step 1  # ← IGNORED! Not in If block
```

✅ **This WORKS:**
```cns
Step 1:
  Then: count becomes count + 1
  If: count < 10
    Then: repeat from Step 1  # ← Works! Inside If block
```

---

## Effects (I/O Operations)

### Print

```cns
Effect: Print "Hello, World!"
Effect: Print "Count: {count}"           # String interpolation
Effect: Print "Name: " + name            # String concatenation
```

### File Operations

```cns
# Read file (returns String)
Then: content becomes READ FROM FILE "/tmp/file.txt"

# Write file (overwrites)
Effect: WRITE "data" TO FILE "/tmp/output.txt"

# Append to file
Effect: APPEND "more data\n" TO FILE "/tmp/output.txt"

# List files in directory (returns List of String)
Effect: LIST FILES IN "/tmp" INTO files
Then: file_count becomes LENGTH OF files

# Check if file exists (returns Boolean)
Then: exists becomes FILE EXISTS "/tmp/file.txt"
If: exists
  Effect: Print "File exists!"

# Delete file
Effect: DELETE FILE "/tmp/old.txt"

# Rename/move file
Effect: RENAME FILE "/tmp/old.txt" TO "/tmp/new.txt"
```

**CRITICAL**: 
- File paths MUST be literal strings in quotes: `"/tmp/file.txt"`
- Cannot use variables: `FILE log_file` ❌
- Cannot concatenate paths: `FILE "/tmp/" + filename` ❌
- If you need dynamic file names, use a fixed path with descriptive name

### HTTP Requests

```cns
# Simple GET
Effect: HTTP GET from url into response

# GET with status code
Effect: HTTP GET from url into response with status code status

# POST with body
Effect: HTTP POST to url with body body_data into response

# Then check status
If: status = 200
  Then: data becomes PARSE JSON response GET "result"
Otherwise:
  Effect: Print "HTTP error: {status}"
```

### Network Server

```cns
Given:
  port: Integer = 8080
  server_socket: Socket
  response: String = ""

# Create server
Step 1 → Start server
  Because: Listen for connections
  Effect: Create socket server_socket on port

# Accept connection
Step 2 → Wait for client
  Because: Get incoming request
  Effect: Accept connection on server_socket

# Read request (auto-populates REQUEST_METHOD, REQUEST_PATH, etc.)
Step 3 → Read HTTP request
  Because: Parse client request
  Effect: Network read
  Then: method becomes REQUEST_METHOD
  Then: path becomes REQUEST_PATH

# Send response
Step 4 → Send response
  Because: Reply to client
  Then: response becomes "HTTP/1.1 200 OK\r\n\r\nHello"
  Effect: Send response to client

# Close connection
Step 5 → Close connection
  Because: Free resources
  Effect: Close connection client_socket

# Loop back
Step 6 → Continue serving
  Because: Handle multiple requests
  If: TRUE
    Then: repeat from Step 2
```

### Database Operations

```cns
# Connect to database
Effect: DATABASE CONNECT TO "/tmp/data.db" AS db

# Execute SQL (no result)
Effect: DATABASE EXECUTE "CREATE TABLE users (id INT, name TEXT)" ON db

# Query SQL (returns result)
Then: rows becomes DATABASE QUERY "SELECT * FROM users" ON db
Effect: Print "Rows: {rows}"

# Close connection
Effect: DATABASE CLOSE db
```

### Process Management (v2.0.0)

```cns
# Launch background job (returns PID)
Then: job_id becomes SHELL "sleep 10" BACKGROUND INTO pid
Effect: Print "Started job: {job_id}"

# Check process status (returns "running", "completed", or "not-found")
Then: status becomes STATUS OF job_id
If: status = "running"
  Effect: Print "Job is running"

# Wait for process to complete (returns exit code)
Then: exit_code becomes WAIT FOR job_id
Effect: Print "Job completed with exit code: {exit_code}"

# Wait with timeout (returns exit code or NIL on timeout)
Then: result becomes WAIT FOR job_id WITH TIMEOUT 5
If: result = NIL
  Effect: Print "Job timed out!"
Otherwise:
  Effect: Print "Job completed: {result}"

# Send signal to process (returns TRUE/FALSE)
Then: killed becomes KILL job_id                    # Default: SIGTERM
Then: killed becomes KILL job_id WITH SIGTERM       # Graceful shutdown
Then: killed becomes KILL job_id WITH SIGKILL       # Force kill
Then: killed becomes KILL job_id WITH SIGINT        # Interrupt
Then: killed becomes KILL job_id WITH SIGHUP        # Hangup

# Example: Parallel execution
Given:
  job1: Number = 0
  job2: Number = 0
  job3: Number = 0

Step 1 → Launch parallel jobs
  Then: job1 becomes SHELL "task1.sh" BACKGROUND INTO pid1
  Then: job2 becomes SHELL "task2.sh" BACKGROUND INTO pid2
  Then: job3 becomes SHELL "task3.sh" BACKGROUND INTO pid3

Step 2 → Wait for all jobs
  Then: WAIT FOR pid1
  Then: WAIT FOR pid2
  Then: WAIT FOR pid3
  Effect: Print "All jobs complete!"
```

**Process Management Notes:**
- Background jobs run independently of the CNS script
- PIDs are stored in both the BECOMES variable and the INTO variable
- WAIT FOR blocks until process completes or timeout expires
- STATUS OF is non-blocking and returns current state
- Completed processes are automatically removed from tracking
- SIGKILL (9) immediately terminates and removes from tracking
- All process operations return values and can be used in expressions

---

## String Operations

```cns
# Case conversion
Then: upper becomes UPPERCASE text
Then: lower becomes LOWERCASE text

# Whitespace
Then: clean becomes TRIM text

# Padding (v1.10.0)
Then: padded becomes PAD text TO 10                    # Right padding (default)
Then: padded becomes PAD text TO 10 LEFT               # Left padding
Then: padded becomes PAD number TO 5 WITH "0"          # Zero padding (right)
Then: padded becomes PAD number TO 5 LEFT WITH "0"     # Zero padding (left)

# Character stripping (v1.10.0)
Then: clean becomes STRIP " " FROM text                # Strip from both sides (default)
Then: clean becomes STRIP " " FROM text LEFT           # Strip from left only
Then: clean becomes STRIP " " FROM text RIGHT          # Strip from right only
Then: clean becomes STRIP "!?" FROM text               # Strip multiple characters

# URL encoding/decoding (v1.10.0)
Then: encoded becomes URL_ENCODE url                   # URL percent-encoding
Then: decoded becomes URL_DECODE encoded               # URL percent-decoding

# Splitting
Then: lines becomes SPLIT text BY "\n"
Then: words becomes SPLIT text BY " "

# Joining
Then: csv becomes JOIN items WITH ","
Then: sentence becomes JOIN words WITH " "

# Replacement
Then: fixed becomes REPLACE "old" WITH "new" IN text

# Concatenation
Then: full_name becomes first + " " + last
```

**v1.10.0 String Utilities:**
- **PAD text TO width [LEFT|RIGHT] [WITH char]**: Pad string to fixed width
  - Default alignment: RIGHT (adds padding on left)
  - Default character: space
  - Custom padding: `PAD number TO 5 WITH "0"` produces "00042"
  - Returns original string if already at or exceeds target width
  
- **STRIP chars FROM text [LEFT|RIGHT]**: Remove characters from string edges
  - Default mode: BOTH (strips from both sides)
  - Supports multiple characters: `STRIP "!? " FROM text`
  - Uses Common Lisp's string-trim functions for efficiency
  
- **URL_ENCODE text**: RFC 3986 compliant URL encoding
  - Preserves unreserved chars: A-Z, a-z, 0-9, -, _, ., ~
  - Encodes others as %XX hex sequences
  - Example: "hello world" → "hello%20world"
  
- **URL_DECODE text**: Decode URL percent-encoded strings
  - Decodes %XX sequences back to characters
  - Converts + to space (query string compatible)
  - Handles malformed encoding gracefully

---

## List Operations

```cns
# Get list length
Then: count becomes LENGTH OF items

# Get first item
Then: first becomes FIRST FROM items

# Add to list
Effect: ADD item TO LIST items

# Reverse list (v1.9.0)
Then: reversed becomes REVERSE items

# Remove duplicates (v1.9.0)
Then: unique becomes UNIQUE items

# Sort list of primitives (v1.9.0)
Then: sorted becomes SORT items

# Sort list of maps by field (v1.9.0)
Then: sorted becomes SORT items BY "field_name"

# Slice/extract subset (v1.9.0)
Then: subset becomes SLICE items FROM 0 TO 5
Then: subset becomes SLICE items FROM start TO end

# Iterate (if supported in your CNS version)
For each item in items:
  Effect: Print "Item: {item}"
```

**v1.9.0 List Operations:**
- **REVERSE**: Returns a new list with elements in reverse order
- **UNIQUE**: Returns a new list with duplicates removed
- **SORT**: Returns a new sorted list (numbers or strings)
- **SORT BY**: Sorts list of maps by specified field name
- **SLICE**: Returns subset of list from start index to end index (0-based)

---

## Map Operations (v1.9.0)

```cns
# Get all keys from a map
Then: all_keys becomes KEYS OF config

# Get all values from a map
Then: all_values becomes VALUES OF config

# Merge two maps (second map overwrites conflicting keys)
Then: combined becomes MERGE defaults WITH overrides
```

**v1.9.0 Map Operations:**
- **KEYS OF**: Returns a list of all keys in the map
- **VALUES OF**: Returns a list of all values in the map
- **MERGE WITH**: Creates a new map combining two maps (right side wins conflicts)

**Note**: Maps are initialized automatically when declared with type `Map`:
```cns
Given:
  config: Map    # Automatically initialized to empty hash table
```

---

## JSON Operations

```cns
# Parse JSON and get field
Then: name becomes PARSE JSON response GET "name"

# Nested fields (use dot notation)
Then: city becomes PARSE JSON response GET "address.city"

# Array access
Then: first_item becomes PARSE JSON response GET "items.0"
```

**Note**: JSON generation not built-in, use string concatenation:
```cns
Then: json becomes "{\"name\":\"" + name + "\",\"age\":" + age + "}"
```

---

## CLI Arguments

### Positional Arguments: `ARGS[n]`

Access command-line arguments by position (zero-based):

```cns
Given:
  source: String = ARGS[0]      # First argument
  dest: String = ARGS[1]        # Second argument
```

Usage: `./cns-run script.cns input.txt output.txt`

**Behavior**:
- Returns empty string `""` if index out of bounds
- Zero-based indexing (first argument is `ARGS[0]`)

### Named Arguments: `ARG("--flag", "default")`

Named arguments with optional defaults:

```cns
Given:
  port: String = ARG("--port", "8080")
  host: String = ARG("--host", "localhost")
```

Usage: `./cns-run script.cns --port 3000 --host 0.0.0.0`

**Supports both syntaxes**:
- `--port 3000` (space-separated)
- `--port=3000` (equals syntax)

**Behavior**:
- Returns default value if flag not provided
- Returns empty string if no default and flag absent

### Boolean Flags: `HAS_FLAG("--flag")`

Check if a flag is present:

```cns
Given:
  verbose: Boolean = HAS_FLAG("--verbose")
  debug: Boolean = HAS_FLAG("--debug")
```

Usage: `./cns-run script.cns --verbose`

**Behavior**:
- Returns `true` if flag present
- Returns `false` if flag absent

### Type Conversion

CLI arguments are strings - convert with:

```cns
# String to integer
Then: port_num becomes PARSE_INT port_str

# String to float
Then: rate_num becomes PARSE_FLOAT rate_str
```

---

## Functions

Functions are reusable Stories marked with `(function)` tag.

### Defining a Function

```cns
Story: Multiply (function)

Given:
  a: Integer        # First parameter
  b: Integer        # Second parameter
  result: Integer = 0

Step 1 → Calculate product
  Because: Multiply the two numbers
  Then: result becomes a * b

End: Return result
```

**Key points**:
- Add `(function)` after function name
- First N variables in `Given:` become parameters (in order)
- `End: Return value` specifies return value
- Functions can be defined in any order

### Calling a Function

```cns
Story: Main Program

Given:
  answer: Integer

Step 1 → Call multiply
  Because: Calculate product
  Then: answer becomes Multiply(10, 20)

End: Return answer
```

**Syntax**: `FunctionName(arg1, arg2, ...)`

### Multiple Functions Per File

Use `---` to separate multiple stories:

```cns
Story: Add (function)
Given:
  x: Integer
  y: Integer
End: Return x + y

---

Story: Multiply (function)
Given:
  a: Integer
  b: Integer
End: Return a * b

---

Story: Main
Given:
  result: Integer
Step 1 → Calculate
  Then: result becomes Add(Multiply(2, 3), 5)
End: Return result
```

**Entry point**: Story WITHOUT `(function)` tag, or first story if all have `(function)`

### Recursion

Functions can call themselves:

```cns
Story: Factorial (function)

Given:
  n: Integer
  result: Integer = 1

Step 1 → Base case
  Because: Factorial of 0 or 1 is 1
  If: n <= 1
    Then: go to End

Step 2 → Recursive case
  Because: n! = n * (n-1)!
  Then: result becomes n * Factorial(n - 1)

End: Return result
```

### Scope Rules

- **Parameters** are local to the function
- **Local variables** declared in Given: are local
- **No global variables** - functions cannot access caller's variables
- **Pass by value** - function receives copy of arguments

---

## Common Patterns

### Pattern 1: Loop with Accumulator

```cns
Story: Sum of Range

Given:
  current: Integer = 1
  limit: Integer = 100
  sum: Integer = 0

Step 1 → Add to sum
  Because: Accumulate total
  Then: sum becomes sum + current
  Then: current becomes current + 1

Step 2 → Check if done
  Because: Stop at limit
  If: current <= limit
    Then: repeat from Step 1

End: Return sum
```

### Pattern 2: Input Validation

```cns
Story: Safe Division

Given:
  numerator: Integer = 100
  denominator: Integer = 5
  result: Integer = 0

Step 1 → Validate denominator
  Because: Prevent division by zero
  If: denominator = 0
    Effect: Print "Error: Cannot divide by zero"
    Then: go to End

Step 2 → Perform division
  Because: Calculate result safely
  Then: result becomes numerator / denominator
  Effect: Print "Result: {result}"

End: Return result
```

### Pattern 3: HTTP API Call with Error Handling

```cns
Story: Fetch User Data

Given:
  url: String = "https://api.example.com/users/1"
  response: String = ""
  status: Integer = 0
  name: String = ""

Step 1 → Call API
  Because: Fetch user data
  Effect: HTTP GET from url into response with status code status

Step 2 → Check success
  Because: Only process successful responses
  If: status = 200
    Then: go to Step 3
  Otherwise:
    Effect: Print "API error: {status}"
    Then: go to End

Step 3 → Parse response
  Because: Extract user name
  Then: name becomes PARSE JSON response GET "name"
  Effect: Print "User: {name}"

End: Return name
```

### Pattern 7: HTTP Server with Routing

```cns
Story: Multi-Route Server

Given:
  port: Integer = 8080
  server_socket: Socket
  response: String = ""
  method: String = ""
  path: String = ""

Step 1 → Start server
  Because: Listen on port
  Effect: Create socket server_socket on port

Step 2 → Accept connection
  Because: Get client request
  Effect: Accept connection on server_socket

Step 3 → Read request
  Because: Parse HTTP request
  Effect: Network read
  Then: method becomes REQUEST_METHOD
  Then: path becomes REQUEST_PATH

Step 4 → Route request
  Because: Direct to appropriate handler
  If: path = "/"
    Then: go to Step 5
  Otherwise:
    Then: go to Step 6

Step 5 → Handle home page
  Because: Serve main page
  Then: response becomes "HTTP/1.1 200 OK\r\n\r\nWelcome"
  Effect: Send response to client
  Then: go to Step 10

Step 6 → Check about page
  Because: Check for /about route
  If: path = "/about"
    Then: go to Step 7
  Otherwise:
    Then: go to Step 8

Step 7 → Handle about page
  Because: Serve about page
  Then: response becomes "HTTP/1.1 200 OK\r\n\r\nAbout Us"
  Effect: Send response to client
  Then: go to Step 10

Step 8 → Handle 404
  Because: Unknown route
  Then: response becomes "HTTP/1.1 404 Not Found\r\n\r\n404"
  Effect: Send response to client

Step 10 → Close and continue
  Because: Clean up and serve next request
  Effect: Close connection client_socket
  If: TRUE
    Then: repeat from Step 2

End: Return 0
```

### Pattern 5: Math Operations

```cns
Story: Mathematical Calculations

Given:
  radius: Integer = 5
  area: Integer = 0
  hypotenuse: Integer = 0
  temperature: Integer = 0
  dice: Integer = 0

Step 1 → Calculate circle area
  Because: Use power for area = radius²
  Then: area becomes POW radius TO 2
  Effect: Print "Area: {area} square units"

Step 2 → Calculate hypotenuse
  Because: Use square root for Pythagorean theorem
  Then: hypotenuse becomes SQRT OF 25
  Effect: Print "Hypotenuse: {hypotenuse}"

Step 3 → Round temperature
  Because: Display rounded celsius value
  Then: temperature becomes ROUND 98.6
  Effect: Print "Temp: {temperature}°F"

Step 4 → Roll dice
  Because: Generate random number 1-6
  Then: dice becomes RANDOM FROM 1 TO 6
  Effect: Print "Rolled: {dice}"

Step 5 → Find range
  Because: Get min and max values
  Effect: Print "Min: {MIN OF 15 AND 28}"
  Effect: Print "Max: {MAX OF 15 AND 28}"

End: Return dice
```

### Pattern 6: File Processing

```cns
Story: Process Text File

Given:
  content: String = ""
  lines: List = []
  count: Integer = 0

Step 1 → Read file
  Because: Load source data
  Then: content becomes READ FROM FILE "/tmp/input.txt"

Step 2 → Split into lines
  Because: Process line by line
  Then: lines becomes SPLIT content BY "\n"

Step 3 → Count lines
  Because: Get line count
  Then: count becomes LENGTH OF lines
  Effect: Print "Lines: {count}"

Step 4 → Write result
  Because: Save count to output
  Effect: WRITE "Line count: {count}" TO FILE "/tmp/output.txt"

End: Return count
```

---

## Common Mistakes to Avoid

1. ❌ Using `==` instead of `=` for comparison
   - ✅ Use: `If: x = 5`
   - ❌ Don't: `If: x == 5`

2. ❌ Using `True`/`False` instead of `TRUE`/`FALSE`
   - ✅ Use: `flag: Boolean = TRUE` (recommended)
   - ✅ Also works: `flag: Boolean = True` (LLM-friendly)
   - ✅ Also works: `flag: Boolean = true` (LLM-friendly)

3. ❌ Putting control flow outside If/Otherwise blocks
   - ✅ Use: `If: done\n  Then: go to End`
   - ❌ Don't: `Then: go to End` (not in If block)

4. ❌ Using functions that don't exist
   - ❌ `NOW()` → ✅ Use `TIMESTAMP()`
   - ❌ `SPLIT(text, ",")` → ✅ Use `SPLIT text BY ","`
   - ❌ `ENV("VAR")` → Not available
   - ❌ `arr[0]` → ✅ Use `FIRST FROM arr`

5. ✅ Literal-first expressions (FIXED!)
   - ✅ Both work: `result becomes n * 3` AND `result becomes 3 * n`
   - **NEW**: As of v1.1, both orders work correctly!

6. ❌ Multi-operator expressions
   - ✅ Use two lines:
     ```
     Then: temp becomes n * 3
     Then: result becomes temp + 1
     ```
   - ❌ Don't: `result becomes n * 3 + 1` (returns NIL)

7. ❌ Declaring auto-populated variables
   - ❌ Don't declare: `REQUEST_METHOD`, `REQUEST_PATH`, etc.
   - ✅ Just use them after `Effect: Network read`

8. ❌ Using array/map indexing syntax
   - ❌ Don't: `arr[0]`, `arr at 0`, `map["key"]`
   - ✅ Use: `FIRST FROM arr`
   - ❌ Don't: `REQUEST_HEADERS GET "X-Real-IP"` (not supported)

9. ❌ Trying to access individual HTTP headers
   - ❌ Don't: `REQUEST_HEADERS["X-Forwarded-For"]`
   - ✅ Use placeholder: `client: String = "unknown"` if client IP needed

10. ❌ Using parentheses for grouping
    - ❌ Don't: `(a + b) / 2`
    - ✅ Use temp variable:
      ```
      Then: sum becomes a + b
      Then: avg becomes sum / 2
      ```

11. ❌ Complex math in string interpolation
    - ❌ Don't: `"Total: {count + 1}"` (use temp variable)
    - ✅ But simple expressions work: `"√16 = {SQRT OF 16}"` (prints "√16 = 4.0")
    - ✅ Variables work: `"Result: {result}"`
    - ✅ Functions work: `"Random: {RANDOM}"`
    - ✅ Best practice: Use temp variables for complex expressions
      ```
      Then: total becomes count + 1
      Effect: Print "Total: {total}"
      ```

12. ❌ Wrong math function syntax
    - ❌ Don't: `SQRT 16` → Use `SQRT OF 16`
    - ❌ Don't: `POW 2 3` → Use `POW 2 TO 3`
    - ❌ Don't: `ABS -5` → Use `ABS OF -5`
    - ✅ Do: Always include keywords (OF, TO, AND)

13. ❌ Using variables in file paths
    - ❌ Old behavior: `FILE log_file` (no longer works)
    - ✅ New: File operations support variables: `WRITE content TO FILE filepath`
    - ✅ Also: `READ FROM FILE filepath` where filepath is a variable

14. ❌ Wrong CLI argument syntax
    - ❌ Don't: `ARG("port")` without default → Use `ARG("--port", "8080")`
    - ❌ Don't: `ARGS(0)` → Use `ARGS[0]` with square brackets
    - ❌ Don't: `IF_FLAG("--verbose")` → Use `HAS_FLAG("--verbose")`

15. ❌ Wrong function syntax
    - ❌ Don't: `Story: Func` → Use `Story: Func (function)` for functions
    - ❌ Don't: `Func arg1 arg2` → Use `Func(arg1, arg2)` with parentheses
    - ❌ Don't: `End:` → Use `End: Return value` in functions

---

## Best Practices

1. **Declare all variables** in Given section before use
2. **Use simple expressions** - one operator per line when in doubt
3. **Variable-first order** - `x * 3` not `3 * x`
4. **Split complex math** - use temporary variables
5. **Add Because clauses** - explain why each step exists
6. **Use string interpolation** - `"Count: {count}"` in Print statements
7. **Put control flow in If blocks** - never in regular Then clauses
8. **Check HTTP status codes** - validate API responses before parsing
9. **Use descriptive variable names** - `user_count` not `uc`
10. **Test incrementally** - start simple, add complexity gradually

---

## Error Handling

### Option 1: Early Exit
```cns
Step 1 → Validate input
  Because: Prevent processing invalid data
  If: input = ""
    Effect: Print "Error: Empty input"
    Then: go to End
```

### Option 2: Error Section (for exceptions)
```cns
Story: Risky Operation

Given:
  result: Integer = 0

Step 1 → Attempt operation
  Because: Try risky calculation
  Then: result becomes risky_function()

Error:
  Effect: Print "Operation failed!"
  Return -1
  Because: Handle runtime exceptions

End: Return result
```

**Note**: Error blocks catch runtime exceptions (file not found, network failure, etc.), NOT logical validation. Use If/Otherwise for input validation.

---

## Complete Example: Multi-Feature Program

```cns
Story: User API Logger

Given:
  port: Integer = 8080
  server_socket: Socket
  method: String = ""
  path: String = ""
  response: String = ""
  timestamp: String = ""
  log_entry: String = ""
  history: String = ""

Step 1 → Start server
  Because: Listen for connections
  Effect: Create socket server_socket on port
  Effect: Print "Server started on port {port}"

Step 2 → Accept connection
  Because: Get incoming request
  Effect: Accept connection on server_socket

Step 3 → Read request
  Because: Parse HTTP request
  Effect: Network read
  Then: method becomes REQUEST_METHOD
  Then: path becomes REQUEST_PATH

Step 4 → Log request
  Because: Track all requests except /logs
  If: NOT (path = "/logs")
    Then: timestamp becomes TIMESTAMP()
    Then: log_entry becomes timestamp + "," + method + "," + path + "\n"
    Effect: APPEND log_entry TO FILE "/tmp/requests.log"

Step 5 → Route request
  Because: Direct to appropriate handler
  If: path = "/"
    Then: go to Step 10
  Otherwise:
    Then: go to Step 20

Step 10 → Handle home
  Because: Serve main page
  Then: response becomes "HTTP/1.1 200 OK\r\n\r\nWelcome to API Logger"
  Effect: Send response to client
  Then: go to Step 100

Step 20 → Check logs endpoint
  Because: Check if requesting logs
  If: path = "/logs"
    Then: go to Step 30
  Otherwise:
    Then: go to Step 40

Step 30 → Handle logs
  Because: Show request history
  Then: history becomes READ FROM FILE "/tmp/requests.log"
  Then: response becomes "HTTP/1.1 200 OK\r\n\r\n" + history
  Effect: Send response to client
  Then: go to Step 100

Step 40 → Handle 404
  Because: Unknown route
  Then: response becomes "HTTP/1.1 404 Not Found\r\n\r\n404 - Not Found"
  Effect: Send response to client

Step 100 → Close and continue
  Because: Clean up and serve next request
  Effect: Close connection client_socket
  If: TRUE
    Then: repeat from Step 2

End: Return 0
```

---

Generate ONLY the CNS code. No markdown fences, no explanations, just pure CNS syntax.
