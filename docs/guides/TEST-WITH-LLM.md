# Testing CNS Generation with LLMs - Quick Start Guide

This guide helps you test CNS code generation with any LLM (Grok, GPT-4, Claude, etc.).

## Quick Test (5 minutes)

### Step 1: Copy the System Prompt

```markdown
You are generating code in Causal Narrative Script (CNS), a language optimized for LLM comprehension.

CNS Structure:
```
Story: [Description]

Given:
  variable: Type = value

Step N → Action
  Because: Explanation
  Effect: Side effect declaration
  Then: State change

End: Return result
  Because: Final explanation
```

Key Rules:
1. Every Step needs Because:
2. Side effects need Effect: declarations
3. All variables declared in Given:
4. Use "repeat from Step N" for loops
5. Use "If condition, Then: action, Otherwise: action" for branches

Example - Webserver:
```
Story: Simple HTTP webserver

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for HTTP connections

Step 2 → Accept connection on server_socket
  Effect: Accept connection on server_socket
  Because: Handle client request

Step 3 → Send response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nHello, World!" to client
  Because: Respond to request

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving

End: Return "done"
```

Generate only valid CNS code.
```

### Step 2: Give it a Task

**Try these prompts:**

1. **Basic:** "Create a webserver on port 3000"
2. **Intermediate:** "Create a webserver with routes for / and /about"
3. **Advanced:** "Create a webserver that counts visits and shows the count"

### Step 3: Validate the Output

Save the LLM's output to a file:
```bash
cat > test.cns << 'EOF'
[paste LLM output here]
EOF
```

Check if it's valid:
```bash
./cns-run test.cns
```

If it's a webserver, test it:
```bash
# Run in background
./cns-run test.cns &

# Test with curl
curl http://localhost:8080/

# Stop server
killall sbcl
```

## Full Evaluation (30 minutes)

### Setup

1. Create test directory:
```bash
mkdir -p llm-tests
cd llm-tests
```

2. Create test harness:
```bash
cat > test-generation.sh << 'EOF'
#!/bin/bash
# Test CNS generation

TEST_FILE="$1"
TEST_NAME=$(basename "$TEST_FILE" .cns)

echo "Testing: $TEST_NAME"

# Validate
if ../cns-run "$TEST_FILE" 2>&1 | grep -q "Error"; then
    echo "❌ FAIL: Execution error"
    exit 1
else
    echo "✅ PASS: Executes successfully"
fi
EOF

chmod +x test-generation.sh
```

### Test Suite

Run these 10 tasks with your LLM:

**Math (2 tasks):**
1. "Calculate factorial of 5"
2. "Check if a number is prime"

**File I/O (2 tasks):**
3. "Read a file and count words"
4. "Write 'Hello World' to output.txt"

**Webserver (6 tasks):**
5. "Create a webserver on port 8080"
6. "Create a webserver with / and /about routes"
7. "Create a webserver that returns JSON"
8. "Create a webserver that handles 404 errors"
9. "Create a webserver that counts requests"
10. "Create a webserver on port 9000 with 3 routes"

### Scoring

For each test, record:
- ✅ / ❌ Parses correctly
- ✅ / ❌ Has all required sections (Story, Given, Steps, End)
- ✅ / ❌ All steps have Because:
- ✅ / ❌ Effects declared correctly
- ✅ / ❌ Executes without errors
- ✅ / ❌ Produces correct output
- Number of iterations needed: ___

**Target scores:**
- Parse rate: ≥95% (9.5/10)
- Execution rate: ≥85% (8.5/10)
- Correctness: ≥80% (8/10)
- Avg iterations: ≤3

## Few-Shot Examples

If zero-shot doesn't work well, try few-shot with these examples:

### Example 1: Factorial
```
Prompt: "Calculate factorial of 5"

CNS:
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1, repeat from Step 1
  Because: Need all factors
  Otherwise: go to End

End: Return result
  Because: Computation complete
```

### Example 2: Webserver
```
Prompt: "Create a webserver on port 8080"

CNS:
Story: Simple HTTP webserver

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client

Step 3 → Send response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nHello!" to client
  Because: Respond to request

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving

End: Return "done"
```

### Example 3: File I/O
```
Prompt: "Count words in a file"

CNS:
Story: Count words in text file

Given:
  filename: String = "input.txt"
  content: String = ""
  word_count: Integer = 0

Step 1 → Read file
  Effect: Read filename into content
  Because: Load text data

Step 2 → Count words
  Because: Calculate word count
  Then: word_count becomes length of split content by spaces

Step 3 → Display result
  Effect: Print "Words: {word_count}"
  Because: Show result

End: Return word_count
```

Then ask: "Now generate CNS for: [new task]"

## Common Issues and Fixes

### Issue: Missing Because clauses

**Bad:**
```
Step 1 → Do something
```

**Good:**
```
Step 1 → Do something
  Because: Explanation of why
```

### Issue: Undefined variables

**Bad:**
```
Step 1 → Use server_socket  # Not declared!
```

**Good:**
```
Given:
  server_socket: Socket

Step 1 → Create server_socket on port
```

### Issue: Missing Effect declarations

**Bad:**
```
Step 1 → Send data to client  # Side effect not declared!
```

**Good:**
```
Step 1 → Send data to client
  Effect: Send "data" to client
  Because: Respond to request
```

### Issue: Implicit loops

**Bad:**
```
Step 2 → Keep accepting connections  # How?
```

**Good:**
```
Step 2 → Accept connection
  Because: Handle client

Step 3 → If TRUE, repeat from Step 2
  Because: Keep serving
```

## Recording Results

Create a results file:
```bash
cat > results.md << 'EOF'
# LLM CNS Generation Test Results

**LLM:** [name and version]
**Date:** [date]
**Method:** [zero-shot / few-shot / fine-tuned]

## Results

| Test | Parse | Valid | Execute | Correct | Iterations |
|------|-------|-------|---------|---------|------------|
| 1. Factorial | ✓ | ✓ | ✓ | ✓ | 1 |
| 2. Prime | ✓ | ✓ | ✓ | ✓ | 2 |
| ... | | | | | |

## Statistics

- Parse rate: __/10 (__%)
- Execution rate: __/10 (__%)
- Correctness: __/10 (__%)
- Average iterations: __

## Notes

[Any observations about the LLM's performance]

## Sample Generated Code

### Best Example
```
[paste best output]
```

### Worst Example
```
[paste worst output]
```

### Common Errors
- [error pattern 1]
- [error pattern 2]
EOF
```

## Comparison Baseline

To validate CNS's value, also test the LLM on equivalent Python tasks:

**Python equivalent:**
```
Task: "Create a web server on port 8080 using Flask"
```

Compare:
- Lines of code
- Iterations needed
- Clarity of errors
- Self-correction ability

**Hypothesis:** CNS should require fewer iterations due to explicit structure.

## Next Steps After Testing

1. **If success rate ≥80%:**
   - Document successful patterns
   - Test more complex tasks
   - Build automated evaluation

2. **If success rate <80%:**
   - Analyze failure patterns
   - Add more training examples for weak areas
   - Refine prompt templates
   - Try few-shot instead of zero-shot

3. **Iterate:**
   - Use failures to generate new training examples
   - Update prompts based on common mistakes
   - Re-test with improved prompts

## Share Results

If testing goes well:
1. Save results to `llm-tests/results.md`
2. Share successes on X with #CNSLang
3. Open issues for systematic failures
4. Contribute successful examples back to dataset

---

**Ready to test? Start with the Quick Test above!** 🚀
