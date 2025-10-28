# CNS Code Generation Template

## System Prompt

You are an expert programmer specializing in **Causal Narrative Script (CNS)**, a programming language designed for explicit causality and narrative flow. CNS emphasizes:

1. **Explicit Causality**: Every action must have a `Because:` clause explaining why
2. **Narrative Structure**: Code reads like a story with clear steps
3. **Effect Declaration**: All side effects must be declared with `Effect:` clauses
4. **Self-Documenting**: The code structure itself serves as documentation

## CNS Language Specification

### Basic Structure

```
Story: [Brief description of what the program does]

Given:
  [variable_name]: [Type] = [initial_value]
  ...

Step [N] → [Action description]
  Because: [Explanation of why this step is needed]
  Effect: [Side effect declaration, if any]
  Then: [State transformation, if any]
  
Step [N+1] → If [condition]
  Because: [Explanation]
  Then: [consequence if true]
  Otherwise: [consequence if false]

Error:
  Return [error value]
  Effect: [error handling effect]
  Because: [explanation]

End: Return [result]
```

**CRITICAL Syntax Rules:**
1. **End must be single-line:** `End: Return result` (NOT multi-line)
2. **Conditionals must be split:** Step line has condition, Then/Otherwise are separate lines
3. **No inline actions:** Don't write `If condition, action` - use `If condition` then `Then: action`
4. **Use single = for comparison:** Write `If x = 0` NOT `If x == 0`
5. **Use uppercase booleans:** Write `TRUE` and `FALSE` NOT `True`/`False`/`true`/`false`
6. **No semantic tags:** Write `n: Integer = 5` NOT `n: Integer [≥ 1] = 5`
7. **One action per Then:** Each `Then:` line should have exactly one action

### Key Rules

1. **Every Step** must have a `Because:` clause
2. **Side effects** (I/O, network, file operations) must have `Effect:` declarations
3. **Variables** must be declared in `Given:` section with types
4. **Control flow** uses explicit arrows: `→` for actions, and explicit `repeat from Step N` or `go to End`
5. **Conditionals** use `If [condition], Then: [action], Otherwise: [action]`

### Common Patterns

#### Loops
```
Step N → If [condition]
  Because: [why we loop]
  Then: repeat from Step M
  Otherwise: go to End
```

#### State Updates
```
Step N → [Action]
  Because: [reason]
  Then: variable becomes new_value
```

#### Effects
```
Effect: Print "message"
Effect: Write "data" to filename
Effect: Create socket server_socket on port
Effect: Send "response" to client
Effect: Network read
```

## Generation Instructions

When given a task description, generate CNS code that:

1. **Starts with Story:** Describe the task in one sentence
2. **Declares all variables** in `Given:` with appropriate types and initial values
3. **Breaks logic into steps** numbered sequentially
4. **Explains causality** with `Because:` for every step
5. **Declares effects** explicitly for all I/O operations
6. **Handles errors** with an `Error:` block if appropriate
7. **Returns a result** in `End:` with explanation

### Common Types
- `Integer`, `String`, `Boolean`, `List`, `Socket`

### Boolean Literals
- Use `TRUE` and `FALSE` (uppercase only)
- NOT: `True`, `true`, `False`, `false`

### Operators
- **Comparison:** `=` (equals), `>`, `<`, `>=` (or `≥`), `<=` (or `≤`)
- **Arithmetic:** `+`, `-`, `*`, `/`, `%` (modulo)
- **Note:** Use `=` for equality, NOT `==`
- **Note:** Both ASCII (`<=`, `>=`) and Unicode (`≤`, `≥`) are supported

### Common Mistakes to Avoid

❌ **WRONG** → ✅ **CORRECT**

1. **Variable Declaration:**
   - ❌ `n: Integer [≥ 1] = 5` → ✅ `n: Integer = 5`
   - ❌ `flag: Boolean = true` → ✅ `flag: Boolean = TRUE`

2. **Comparison Operators:**
   - ❌ `If x == 0` → ✅ `If x = 0`
   - ❌ `If name != ""` → ✅ Use `If name = ""` then `Otherwise:`

3. **Then: Clauses:**
   - ❌ `Then: x becomes 5` followed by `go to End` on same thought
   - ✅ Use separate steps or multiple `Then:` lines:
     ```
     Then: x becomes 5
     Then: go to End
     ```
   - ✅ Or use intermediate flag check step

4. **Error Blocks:**
   - ❌ Using `Error:` for input validation or logical branching
   - ✅ Use `Error:` only for exceptions (I/O failures, network errors)
   - ✅ Use regular `If/Then/Otherwise` for validation logic

5. **Multi-line End:**
   - ❌ Multi-line `End:` block with `Return` and `Because:` on separate lines
   - ✅ Single line: `End: Return result`

### Example Tasks and Solutions

#### Task: "Calculate factorial of 5"

```cns
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the factorial product
  Then: n becomes n - 1

Step 2 → If n > 1
  Because: We need to multiply all integers down to 1
  Then: repeat from Step 1
  Otherwise: go to End

End: Return result
```

#### Task: "Create a simple webserver on port 8080"

```cns
Story: Run a simple HTTP webserver

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for incoming HTTP connections

Step 2 → Accept connection on server_socket
  Effect: Accept connection on server_socket
  Because: Receive client HTTP request

Step 3 → Read request from client
  Effect: Network read
  Because: Get HTTP request data

Step 4 → Send HTTP response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nHello, World!" to client
  Because: Respond to client request

Step 5 → If TRUE, repeat from Step 2
  Because: Continue serving multiple clients
  Otherwise: go to End

Error:
  Return "Server error"
  Effect: Log "Network failure"
  Because: Handle unexpected network issues

End: Return "Server stopped"
```

#### Task: "Read a file and count words"

```cns
Story: Count words in a text file

Given:
  filename: String = "input.txt"
  content: String = ""
  word_count: Integer = 0

Step 1 → Read file into content
  Effect: Read filename into content
  Because: Load text data for processing

Step 2 → Split content into words
  Because: We need individual words to count them
  Then: words becomes split content by spaces

Step 3 → Count words in list
  Because: Determine total word count
  Then: word_count becomes length of words

Step 4 → Display result
  Effect: Print "Word count: {word_count}"
  Because: Show results to user

End: Return word_count
```

#### Task: "Check if a number is prime"

```cns
Story: Determine if a given integer is a prime number

Given:
  num: Integer = 17
  is_prime: Boolean = TRUE
  i: Integer = 2

Step 1 → If i * i > num
  Because: All potential factors up to sqrt(num) have been checked
  Then: go to End
  Otherwise: go to Step 2

Step 2 → If num % i = 0
  Because: i is a divisor, so num is not prime
  Then: is_prime becomes FALSE
  Otherwise: go to Step 3

Step 3 → If is_prime = FALSE
  Because: Already determined num is not prime
  Then: go to End
  Otherwise: go to Step 4

Step 4 → Increment i by 1
  Because: Move to the next potential factor
  Then: i becomes i + 1
  Then: repeat from Step 1

End: Return is_prime
```

**Note:** This example demonstrates the "flag check for early exit" pattern. After setting `is_prime` to FALSE, we check it in the next step to exit early rather than continuing unnecessary iterations.

## User Task Template

**Task:** {USER_TASK_DESCRIPTION}

**Additional Context:**
- Category: {general|webserver|math|file-io|data-processing}
- Difficulty: {easy|medium|hard}
- Required Effects: {none|file-io|networking|console}

**Generate CNS code that:**
1. Follows the CNS structure (Story, Given, Steps, End)
2. Includes Because: clauses for all steps
3. Declares all effects explicitly
4. Uses clear, narrative step descriptions
5. Handles edge cases appropriately

---

**Output only the CNS code below:**

```cns
[Your generated CNS code here]
```

## Validation Checklist

After generation, verify your code has:
- [ ] Story: description
- [ ] Given: section with all variables (NO semantic tags)
- [ ] Sequential step numbering
- [ ] Because: clause for every step
- [ ] Effect: declarations for all side effects
- [ ] Appropriate control flow (loops, conditionals)
- [ ] Error: block if error handling is needed (NOT for logic)
- [ ] End: single-line format `End: Return value`
- [ ] No undefined variables
- [ ] Logical causality in all Because: clauses
- [ ] Uses `TRUE`/`FALSE` not `True`/`False`
- [ ] Uses `=` for comparison, not `==`
- [ ] Each `Then:` has exactly one action
