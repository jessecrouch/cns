# CNS Common Patterns

**Purpose**: Reusable code patterns and idioms for common programming tasks.

**Audience**: LLMs generating CNS code, developers learning CNS patterns.

**Organization**: Grouped by use case with copy-paste ready templates.

---

## Table of Contents

1. [Read-Process-Write](#read-process-write)
2. [API-Parse-Store](#api-parse-store)
3. [Validate-Process-Return](#validate-process-return)
4. [Loop-Accumulate](#loop-accumulate)
5. [File Processing](#file-processing)
6. [HTTP Patterns](#http-patterns)
7. [Database Patterns](#database-patterns)
8. [Error Handling](#error-handling)
9. [Text Processing](#text-processing)
10. [Multi-API Orchestration](#multi-api-orchestration)

---

## Read-Process-Write

**Use case:** Read input, transform data, write output.

### Template

```cns
Story: Process Data File

Given:
  input_file: String = "/tmp/input.txt"
  output_file: String = "/tmp/output.txt"
  content: String = ""
  processed: String = ""

Step 1 → Read input
  Because: Load source data
  Then: content becomes READ FROM FILE input_file

Step 2 → Process content
  Because: Transform data
  Then: processed becomes UPPERCASE content

Step 3 → Write output
  Because: Save results
  Effect: WRITE processed TO FILE output_file

End: Return 0
```

### Real Example: Convert CSV

```cns
Story: Convert CSV to Uppercase Names

Given:
  input: String = "/tmp/names.csv"
  output: String = "/tmp/names_upper.csv"
  data: String = ""
  lines: List = []
  result: List = []

Step 1 → Read CSV
  Then: data becomes READ FROM FILE input

Step 2 → Split into lines
  Then: lines becomes SPLIT data BY "\n"

Step 3 → Process each line
  For each line in lines:
    Then: upper becomes UPPERCASE line
    Then: result becomes result + [upper]

Step 4 → Write output
  Then: output_text becomes JOIN result BY "\n"
  Effect: WRITE output_text TO FILE output

End: Return 0
```

---

## API-Parse-Store

**Use case:** Call REST API, parse JSON response, store results.

### Template

```cns
Story: Fetch and Store API Data

Given:
  api_url: String = "https://api.example.com/data"
  response: String = ""
  status: Integer = 0
  data: String = ""
  storage_file: String = "/tmp/api_cache.txt"

Step 1 → Call API
  Because: Fetch fresh data
  Effect: HTTP GET from api_url into response with status code status

Step 2 → Validate response
  Because: Ensure API call succeeded
  If: status != 200
    Effect: Print "API call failed: {status}"
    Then: go to End

Step 3 → Parse JSON
  Because: Extract needed fields
  Then: data becomes PARSE JSON response GET "result.data"

Step 4 → Store locally
  Because: Cache for later use
  Effect: WRITE data TO FILE storage_file

End: Return data
```

### Real Example: GitHub User Info

```cns
Story: Get GitHub User Info

Given:
  username: String = "octocat"
  url: String = ""
  response: String = ""
  status: Integer = 0
  name: String = ""
  repos: Integer = 0

Step 1 → Build URL
  Then: url becomes "https://api.github.com/users/" + username

Step 2 → Call GitHub API
  Effect: HTTP GET from url into response with status code status

Step 3 → Check success
  If: status != 200
    Effect: Print "Failed to fetch user {username}"
    Then: go to End

Step 4 → Parse user data
  Then: name becomes PARSE JSON response GET "name"
  Then: repos becomes PARSE JSON response GET "public_repos"
  Effect: Print "{name} has {repos} public repos"

End: Return repos
```

---

## Validate-Process-Return

**Use case:** Validate input, process if valid, return result or error.

### Template

```cns
Story: Safe Processing

Given:
  input: String = ""
  result: String = ""
  is_valid: Integer = 0

Step 1 → Validate input
  Because: Prevent processing invalid data
  If: input == ""
    Effect: Print "Error: Empty input"
    Then: go to End
  Otherwise:
    Then: is_valid becomes 1

Step 2 → Process valid input
  Because: Transform validated data
  Then: result becomes UPPERCASE input

End: Return result
```

### Real Example: Division with Validation

```cns
Story: Safe Division

Given:
  numerator: Integer = 100
  denominator: Integer = 0
  result: Integer = 0

Step 1 → Validate denominator
  Because: Prevent division by zero
  If: denominator == 0
    Effect: Print "Error: Cannot divide by zero"
    Then: go to End

Step 2 → Perform division
  Because: Calculate result safely
  Then: result becomes numerator / denominator
  Effect: Print "Result: {result}"

End: Return result
```

---

## Loop-Accumulate

**Use case:** Loop over range or list, accumulate results.

### Pattern 1: Count Loop with Accumulator

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

### Pattern 2: List Iteration with Accumulator

```cns
Story: Sum List Values

Given:
  numbers: List = [10, 20, 30, 40, 50]
  total: Integer = 0

Step 1 → Sum all numbers
  Because: Calculate total of all values
  For each num in numbers:
    Then: total becomes total + num

End: Return total
```

### Pattern 3: Conditional Accumulation

```cns
Story: Count Even Numbers

Given:
  numbers: List = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  even_count: Integer = 0
  remainder: Integer = 0

Step 1 → Count evens
  Because: Track even number count
  For each num in numbers:
    Then: remainder becomes num % 2
    If: remainder == 0
      Then: even_count becomes even_count + 1

End: Return even_count
```

---

## File Processing

### Pattern 1: Line-by-Line Processing

```cns
Story: Count Non-Empty Lines

Given:
  filename: String = "/tmp/data.txt"
  content: String = ""
  lines: List = []
  count: Integer = 0

Step 1 → Read file
  Then: content becomes READ FROM FILE filename

Step 2 → Split into lines
  Then: lines becomes SPLIT content BY "\n"

Step 3 → Count non-empty lines
  For each line in lines:
    Then: trimmed becomes TRIM line
    If: trimmed != ""
      Then: count becomes count + 1

End: Return count
```

### Pattern 2: File Transformation

```cns
Story: Replace Text in File

Given:
  input_file: String = "/tmp/input.txt"
  output_file: String = "/tmp/output.txt"
  content: String = ""
  replaced: String = ""

Step 1 → Read source
  Then: content becomes READ FROM FILE input_file

Step 2 → Replace text
  Then: replaced becomes REPLACE "old_text" WITH "new_text" IN content

Step 3 → Write result
  Effect: WRITE replaced TO FILE output_file

End: Return 0
```

### Pattern 3: Multi-File Processing

```cns
Story: Process All CNS Files

Given:
  directory: String = "examples"
  files: List = []
  count: Integer = 0

Step 1 → Find all CNS files
  Because: Discover files to process
  Then: files becomes FIND "*.cns" IN directory WITH COUNT count
  Effect: Print "Found {count} CNS files"

Step 2 → Process each file
  Because: Transform all files
  For each file in files:
    Then: content becomes READ FROM FILE file
    Effect: Print "Processing {file}"
    # Add processing logic here

End: Return count
```

---

## HTTP Patterns

### Pattern 1: GET with Error Handling

```cns
Story: Safe HTTP GET

Given:
  url: String = "https://api.example.com/data"
  response: String = ""
  status: Integer = 0
  data: String = ""

Step 1 → Call API
  Effect: HTTP GET from url into response with status code status

Step 2 → Check HTTP status
  If: status == 200
    Then: data becomes PARSE JSON response GET "data"
  Otherwise:
    Effect: Print "HTTP error: {status}"

End: Return data
```

### Pattern 2: POST with JSON Body

```cns
Story: Submit Form Data

Given:
  url: String = "https://api.example.com/submit"
  name: String = "Alice"
  email: String = "alice@example.com"
  body: String = ""
  response: String = ""

Step 1 → Build JSON body
  Then: body becomes "{\"name\": \"" + name + "\", \"email\": \"" + email + "\"}"

Step 2 → POST data
  Effect: HTTP POST to url with body body into response
  Effect: Print "Response: {response}"

End: Return 0
```

### Pattern 3: Retry on Failure

```cns
Story: HTTP with Retry

Given:
  url: String = "https://api.example.com/data"
  response: String = ""
  status: Integer = 0
  retry_count: Integer = 0
  max_retries: Integer = 3

Step 1 → Attempt request
  Effect: HTTP GET from url into response with status code status

Step 2 → Check success
  If: status == 200
    Then: go to End

Step 3 → Check retry limit
  Then: retry_count becomes retry_count + 1
  If: retry_count < max_retries
    Effect: Print "Retry {retry_count}/{max_retries}"
    Then: repeat from Step 1
  Otherwise:
    Effect: Print "Failed after {retry_count} retries"

End: Return response
```

---

## Database Patterns

### Pattern 1: Simple CRUD Workflow

```cns
Story: User CRUD Operations

G: db_path:S="/tmp/users.db"

S1→ Connect to database
  → DATABASE CONNECT TO db_path AS db

S2→ Create table
  → DATABASE EXECUTE "CREATE TABLE IF NOT EXISTS users (id INT, name TEXT)" ON db

S3→ Insert user
  → DATABASE EXECUTE "INSERT INTO users VALUES (1, 'Alice')" ON db

S4→ Query users
  → results = DATABASE QUERY "SELECT * FROM users" ON db
  → Print "Users: " + results

E: results
```

### Pattern 2: Parameterized Insert

```cns
Story: Insert User with Variables

Given:
  db_path: String = "/tmp/users.db"
  user_id: Integer = 1
  user_name: String = "Bob"
  query: String = ""

Step 1 → Connect
  Effect: DATABASE CONNECT TO db_path AS db

Step 2 → Build insert query
  Then: query becomes "INSERT INTO users VALUES (" + user_id + ", '" + user_name + "')"

Step 3 → Execute insert
  Effect: DATABASE EXECUTE query ON db

End: Return 0
```

---

## Error Handling

### Pattern 1: Error Section

```cns
Story: Safe Operation

Given:
  result: Integer = 0

Step 1 → Risky operation
  Then: result becomes risky_calculation()

Error:
  Effect: Print "Operation failed!"
  Return -1

End: Return result
```

### Pattern 2: Early Exit on Error

```cns
Story: Validate and Process

Given:
  input: String = ""
  result: String = ""

Step 1 → Validate
  If: input == ""
    Effect: Print "Error: Empty input"
    Then: go to End

Step 2 → Process
  Then: result becomes UPPERCASE input

End: Return result
```

---

## Text Processing

### Pattern 1: Extract Data with Regex

```cns
Story: Extract Emails from Text

Given:
  text: String = "Contact: alice@example.com or bob@test.com"
  pattern: String = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
  matches: List = []

Step 1 → Extract emails
  Then: matches becomes EXTRACT pattern FROM text
  Effect: Print "Found emails: {matches}"

End: Return matches
```

### Pattern 2: Text Normalization

```cns
Story: Normalize Text

Given:
  input: String = "  Hello   World  "
  normalized: String = ""

Step 1 → Trim whitespace
  Then: normalized becomes TRIM input

Step 2 → Convert to lowercase
  Then: normalized becomes LOWERCASE normalized

Step 3 → Replace spaces
  Then: normalized becomes REPLACE " " WITH "_" IN normalized

End: Return normalized
```

---

## Multi-API Orchestration

### Template: Sequential API Calls

```cns
Story: Multi-API Workflow

Given:
  api1_url: String = "https://api1.example.com/data"
  api2_url: String = "https://api2.example.com/process"
  response1: String = ""
  response2: String = ""
  status1: Integer = 0
  status2: Integer = 0
  data: String = ""
  result: String = ""

Step 1 → Call first API
  Because: Get initial data
  Effect: HTTP GET from api1_url into response1 with status code status1
  If: status1 != 200
    Effect: Print "API 1 failed"
    Then: go to End

Step 2 → Extract data from first API
  Because: Get data for second call
  Then: data becomes PARSE JSON response1 GET "data"

Step 3 → Call second API with first API's data
  Because: Process data from API 1
  Then: body becomes "{\"input\": \"" + data + "\"}"
  Effect: HTTP POST to api2_url with body body into response2
  If: status2 != 200
    Effect: Print "API 2 failed"
    Then: go to End

Step 4 → Parse final result
  Because: Extract processed result
  Then: result becomes PARSE JSON response2 GET "result"

End: Return result
```

### Real Example: IP Geolocation + Weather

```cns
Story: Get Local Weather

Given:
  ip_url: String = "https://api.ipify.org?format=json"
  geo_url: String = "http://ip-api.com/json/"
  ip_response: String = ""
  geo_response: String = ""
  ip: String = ""
  city: String = ""

Step 1 → Get my IP address
  Effect: HTTP GET from ip_url into ip_response
  Then: ip becomes PARSE JSON ip_response GET "ip"
  Effect: Print "My IP: {ip}"

Step 2 → Get geolocation
  Then: geo_url becomes geo_url + ip
  Effect: HTTP GET from geo_url into geo_response
  Then: city becomes PARSE JSON geo_response GET "city"
  Effect: Print "Location: {city}"

End: Return city
```

---

## Code Navigation Patterns

### Pattern 1: Find and GREP

```cns
Story: Find TODOs in Codebase

Given:
  source_dir: String = "src"
  files: List = []
  todos: List = []
  file_count: Integer = 0
  todo_count: Integer = 0

Step 1 → Find all source files
  Because: Discover files to search
  Then: files becomes FIND "*.lisp" IN source_dir WITH COUNT file_count
  Effect: Print "Searching {file_count} files"

Step 2 → Search for TODOs
  Because: Find all TODO comments
  Then: todos becomes GREP "TODO" IN files WITH COUNT todo_count
  Effect: Print "Found {todo_count} TODOs"

Step 3 → Report results
  Because: Show what was found
  For each todo in todos:
    Effect: Print "{todo}"

End: Return todo_count
```

### Pattern 2: Git Workflow

```cns
Story: Check and Commit Changes

Given:
  repo_path: String = "/tmp/repo"
  status: String = ""
  has_changes: Integer = 0

Step 1 → Check git status
  Because: See what changed
  Then: status becomes GIT STATUS IN repo_path
  Effect: Print "Status:\n{status}"

Step 2 → Check for changes
  Because: Only commit if there are changes
  If: CONTAINS "modified" IN status
    Then: has_changes becomes 1

Step 3 → Add and commit
  Because: Save changes
  If: has_changes == 1
    Effect: GIT ADD "." IN repo_path
    Effect: GIT COMMIT "Auto-commit" IN repo_path
    Effect: Print "Changes committed"
  Otherwise:
    Effect: Print "No changes to commit"

End: Return has_changes
```

---

## Best Practices

### 1. Always Validate External Data

```cns
# ✅ Good - check status before parsing
Step 1 → Call API
  Effect: HTTP GET from url into response with status code status
  
Step 2 → Validate response
  If: status == 200
    Then: data becomes PARSE JSON response
  Otherwise:
    Effect: Print "API failed: {status}"
    Then: go to End
```

### 2. Use Temporary Variables for Complex Expressions

```cns
# ✅ Good - split multi-operator expressions
Then: temp becomes count * 2
Then: result becomes temp + offset

# ❌ Avoid - may return NIL
Then: result becomes count * 2 + offset
```

### 3. Add Because Clauses for Clarity

```cns
# ✅ Good - explains step purpose
Step 1 → Validate input
  Because: Prevent processing invalid data
  If: input == ""
    Then: go to End
```

### 4. Use Waterfall Pattern for Multiple Conditions

```cns
# ✅ Good - sequential checks
Step 1 → Check condition A
  If: a == true
    Then: go to Step 10

Step 2 → Check condition B
  If: b == true
    Then: go to Step 10

Step 10 → Process
  Then: result becomes process()
```

### 5. Separate Read-Transform-Write Steps

```cns
# ✅ Good - clear phases
Step 1 → Read
  Then: data becomes READ FROM FILE input

Step 2 → Transform
  Then: processed becomes UPPERCASE data

Step 3 → Write
  Effect: WRITE processed TO FILE output
```

---

## See Also

- **SYNTAX.md** - Complete syntax reference
- **examples/README.md** - Pattern guide with working examples
- **examples/core/** - Fundamental patterns
- **examples/features/** - Feature-specific patterns
- **examples/advanced/** - Complex multi-feature patterns

---

*Last Updated: 2025-11-02*  
*Status: Complete pattern library*  
*Audience: LLMs and developers*
