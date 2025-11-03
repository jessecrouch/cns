# CNS Complete Reference for LLMs

Task: Build a web server in CNS that logs all incoming HTTP requests to a CSV file and provides an endpoint to view the request history.

## Requirements

1. **Main Endpoint (GET /)**
   - Respond with "Request Logger Server"
   - Log this request to CSV

2. **History Endpoint (GET /history)**
   - Read the CSV file
   - Display all logged requests in a formatted way
   - DO NOT log this request (to avoid infinite logging)

3. **CSV Logging**
   - File: `requests.csv`
   - Headers: `timestamp,method,path,client`
   - Format: Use proper CSV format with quotes if needed
   - Timestamp: Use CNS datetime functions to get current time

4. **Information to Log**
   - Timestamp: Current date and time
   - Method: GET, POST, etc.
   - Path: The requested URL path
   - Client: The client socket/connection info

Generate a complete CNS program that solves this task.

---

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
| Split string | `SPLIT text BY "\n"` | `SPLIT(text, "\n")`, `text.split()` |
| Join list | `JOIN items WITH ","` | `JOIN(items, ",")`, `",".join()` |
| Replace text | `REPLACE "old" WITH "new" IN text` | `REPLACE(text, "old", "new")` |
| **Lists** |
| Get length | `LENGTH OF items` | `LEN(items)`, `items.length`, `SIZE(items)` |
| Get first item | `FIRST FROM items` | `items[0]`, `items at 0`, `HEAD(items)` |
| Add to list | `Effect: ADD item TO LIST items` | `items.append()`, `items.push()` |
| **JSON** |
| Parse JSON | `PARSE JSON response GET "key"` | `JSON.parse()`, `PARSE(response)` |
| Nested field | `PARSE JSON data GET "user.name"` | `data["user"]["name"]` |
| **Files** |
| Read file | `READ FROM FILE "path"` | `READ("path")`, `READFILE("path")` |
| Write file | `Effect: WRITE "data" TO FILE "path"` | `WRITE("data", "path")` |
| Append file | `Effect: APPEND "data" TO FILE "path"` | `APPEND("data", "path")` |
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
| **Environment/System** |
| Get env var | ❌ Not available | `ENV()`, `os.getenv()` |
| Run shell | ❌ Not available | `SHELL()`, `system()` |
| **Comparisons** |
| Equals | `If: x = 5` | `If: x == 5` |
| Not equals | `If: NOT (x = 5)` | `If: x != 5`, `If: x <> 5` |
| Contains | ❌ Use exact `=` | `CONTAINS`, `IN` |
| **Math** |
| Square root | `SQRT x` | `SQRT(x)`, `Math.sqrt()` |
| Power | `x ** y` or loop | `POW(x, y)`, `POWER()` |
| Modulo | `x % y` | `MOD(x, y)` |

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
- [ ] Booleans are `TRUE`/`FALSE` (uppercase, not True/False)
- [ ] All control flow (`go to`, `repeat from`) inside If/Otherwise blocks
- [ ] No literal-first expressions (`n * 3` not `3 * n`)
- [ ] No multi-operator math (`x * 3 + 1` split into two lines)
- [ ] No parentheses in expressions (use temp variables)
- [ ] No functions from other languages (`NOW()`, `SPLIT()`, `ENV()`)

**Variables:**
- [ ] All variables declared in `Given:` section
- [ ] Types specified: Integer, String, List, Map, Socket
- [ ] Auto-populated variables NOT declared (REQUEST_METHOD, REQUEST_PATH, etc.)
- [ ] No array indexing syntax (`arr[0]` or `arr at 0`)

**Effects:**
- [ ] Print uses string interpolation: `"Count: {count}"`
- [ ] File paths are literal strings, not variables
- [ ] HTTP effects use full syntax: `HTTP GET from url into response`
- [ ] Network effects in correct order: Create → Accept → Read → Send → Close

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

**Rules:**
- ALL variables must be declared before use
- Initial values required (except Socket)
- Use `TRUE`/`FALSE` for booleans (uppercase!)

---

## Built-in Variables (DO NOT DECLARE)

After `Effect: Network read`, these are automatically populated:

| Variable | Contains | Example |
|----------|----------|---------|
| `REQUEST_METHOD` | HTTP method | `"GET"`, `"POST"` |
| `REQUEST_PATH` | URL path | `"/"`, `"/api/users"` |
| `REQUEST_BODY` | POST data | `"{\"name\":\"Alice\"}"` |
| `REQUEST_HEADERS` | Header map | `{"Content-Type": "..."}` |
| `REQUEST_QUERY` | Query string | `"?id=123"` |
| `HTTP_STATUS` | Response code | `200`, `404`, `500` |

**Usage:**
```cns
Step 1 → Read HTTP request
  Effect: Network read
  Then: method becomes REQUEST_METHOD  # Don't declare REQUEST_METHOD!
  Then: path becomes REQUEST_PATH
```

**CRITICAL**: Do NOT declare these in `Given:` section!

---

## Expressions

### ✅ SAFE (Always Works)

```cns
Then: result becomes x + 5        # Variable-first
Then: sum becomes a + b           # Simple binary operation
Then: product becomes x * y       # Multiplication
Then: quotient becomes x / 2      # Division
Then: remainder becomes x % 3     # Modulo
```

### ❌ DANGEROUS (Returns NIL or Wrong Result)

```cns
Then: result becomes 3 * n        # ❌ Literal-first → NIL
Then: result becomes n * 3 + 1    # ❌ Multi-operator → NIL
Then: avg becomes (a + b) / 2     # ❌ Parentheses → NIL
Then: result becomes 2 + 3 * 4    # ❌ Wrong order (left-to-right)
```

### ✅ WORKAROUNDS

```cns
# Fix literal-first:
Then: result becomes n * 3        # Swap to variable-first

# Fix multi-operator:
Then: temp becomes n * 3
Then: result becomes temp + 1     # Split into steps

# Fix parentheses:
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
If: x = 5           # Equal (single =, not ==)
If: x > 10          # Greater than
If: x < 100         # Less than
If: x >= 50         # Greater or equal
If: x <= 200        # Less or equal
If: NOT (x = 5)     # Not equal (no != operator)
```

**CRITICAL**: Use `=` for comparison, NOT `==`

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
Effect: WRITE "data" TO FILE "/tmp/file.txt"

# Append to file
Effect: APPEND "more data" TO FILE "/tmp/file.txt"
```

**Note**: File paths must be literal strings, not variables (current limitation)

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

---

## String Operations

```cns
# Case conversion
Then: upper becomes UPPERCASE text
Then: lower becomes LOWERCASE text

# Whitespace
Then: clean becomes TRIM text

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

---

## List Operations

```cns
# Get list length
Then: count becomes LENGTH OF items

# Get first item
Then: first becomes FIRST FROM items

# Add to list
Effect: ADD item TO LIST items

# Iterate (if supported in your CNS version)
For each item in items:
  Effect: Print "Item: {item}"
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

### Pattern 4: HTTP Server with Routing

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

### Pattern 5: File Processing

```cns
Story: Process Text File

Given:
  input_file: String = "/tmp/input.txt"
  content: String = ""
  lines: List = []
  count: Integer = 0

Step 1 → Read file
  Because: Load source data
  Then: content becomes READ FROM FILE input_file

Step 2 → Split into lines
  Because: Process line by line
  Then: lines becomes SPLIT content BY "\n"

Step 3 → Count lines
  Because: Get line count
  Then: count becomes LENGTH OF lines
  Effect: Print "Lines: {count}"

End: Return count
```

---

## Common Mistakes to Avoid

1. ❌ Using `==` instead of `=` for comparison
   - ✅ Use: `If: x = 5`
   - ❌ Don't: `If: x == 5`

2. ❌ Using `True`/`False` instead of `TRUE`/`FALSE`
   - ✅ Use: `flag: Boolean = TRUE`
   - ❌ Don't: `flag: Boolean = True`

3. ❌ Putting control flow outside If/Otherwise blocks
   - ✅ Use: `If: done\n  Then: go to End`
   - ❌ Don't: `Then: go to End` (not in If block)

4. ❌ Using functions that don't exist
   - ❌ `NOW()` → ✅ Use `TIMESTAMP()`
   - ❌ `SPLIT(text, ",")` → ✅ Use `SPLIT text BY ","`
   - ❌ `ENV("VAR")` → Not available
   - ❌ `arr[0]` → ✅ Use `FIRST FROM arr`

5. ❌ Literal-first expressions
   - ✅ Use: `result becomes n * 3`
   - ❌ Don't: `result becomes 3 * n` (returns NIL)

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

8. ❌ Using array indexing syntax
   - ❌ Don't: `arr[0]`, `arr at 0`
   - ✅ Use: `FIRST FROM arr`

9. ❌ Using parentheses for grouping
   - ❌ Don't: `(a + b) / 2`
   - ✅ Use temp variable:
     ```
     Then: sum becomes a + b
     Then: avg becomes sum / 2
     ```

10. ❌ Math in string interpolation
    - ❌ Don't: `"Total: {count + 1}"` (won't evaluate)
    - ✅ Do:
      ```
      Then: total becomes count + 1
      Effect: Print "Total: {total}"
      ```

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
  log_file: String = "requests.log"
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
    Effect: APPEND "{timestamp},{method},{path}" TO FILE log_file

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
  Then: history becomes READ FROM FILE log_file
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
