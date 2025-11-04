#!/bin/bash

# Generate complete prompts with SYNTAX.md embedded for Grok testing

ITERATION_DIR="/home/bolt/Documents/cns/tests/grok-iterations/iteration-4"
SYNTAX_FILE="/home/bolt/Documents/cns/SYNTAX.md"

echo "Generating prompts with embedded SYNTAX.md..."
echo "SYNTAX.md size: $(wc -l < $SYNTAX_FILE) lines"
echo ""

# Generate Test 1 prompt
echo "=== Generating TEST-1-FULL-PROMPT.md ==="
cat > "$ITERATION_DIR/TEST-1-FULL-PROMPT.md" << 'PROMPT_END'
# Test 1: Word Counter CLI Tool

## Task

Build a command-line word counter tool in CNS that:

1. **Accepts a filename as first positional argument** (ARGS[0])
2. **Accepts optional flags:**
   - `--lines` - Show line count instead of word count
   - `--chars` - Show character count instead of word count
   - `--verbose` - Show filename in output
3. **Default behavior** (no flags): Show word count
4. **Handles missing file** - Print error message if file doesn't exist

## Example Usage

```bash
# Count words in file (default)
./cns-run word-counter.cns input.txt
# Output: 42

# Count lines
./cns-run word-counter.cns input.txt --lines
# Output: 10

# Count characters
./cns-run word-counter.cns input.txt --chars
# Output: 256

# Verbose mode
./cns-run word-counter.cns input.txt --verbose
# Output: input.txt: 42 words

# Verbose with lines
./cns-run word-counter.cns input.txt --lines --verbose
# Output: input.txt: 10 lines

# Missing file
./cns-run word-counter.cns nonexistent.txt
# Output: Error: File not found
```

---

**Generate a complete CNS program that solves this task using the syntax reference below.**

PROMPT_END

# Append SYNTAX.md
cat "$SYNTAX_FILE" >> "$ITERATION_DIR/TEST-1-FULL-PROMPT.md"
echo "Generated: TEST-1-FULL-PROMPT.md"

# Generate Test 2 prompt
echo "=== Generating TEST-2-FULL-PROMPT.md ==="
cat > "$ITERATION_DIR/TEST-2-FULL-PROMPT.md" << 'PROMPT_END'
# Test 2: Background Job Manager CLI

## Task

Build a job queue manager CLI tool in CNS that can:

1. **Launch background jobs** - Run shell commands in background
2. **Check job status** - Show if job is running, completed, or not found
3. **Wait for job completion** - Wait for specific job to finish
4. **Kill jobs** - Stop running jobs by PID

## Commands

### 1. `run <command>`
Launch a command in the background and print its PID.

```bash
./cns-run job-manager.cns run "sleep 10"
# Output: Job started: PID 12345
```

### 2. `status <pid>`
Check the status of a job by PID.

```bash
./cns-run job-manager.cns status 12345
# Output: Job 12345: running
```

### 3. `wait <pid>`
Wait for a job to complete and show exit code.

```bash
./cns-run job-manager.cns wait 12345
# Output: Job 12345 completed with exit code: 0
```

### 4. `kill <pid>`
Kill a running job.

```bash
./cns-run job-manager.cns kill 12345
# Output: Job 12345 killed successfully
```

---

**Generate a complete CNS program that solves this task using the syntax reference below.**

PROMPT_END

cat "$SYNTAX_FILE" >> "$ITERATION_DIR/TEST-2-FULL-PROMPT.md"
echo "Generated: TEST-2-FULL-PROMPT.md"

# Generate Test 3 prompt (simplified for size)
echo "=== Generating TEST-3-FULL-PROMPT.md ==="
cat > "$ITERATION_DIR/TEST-3-FULL-PROMPT.md" << 'PROMPT_END'
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

PROMPT_END

cat "$SYNTAX_FILE" >> "$ITERATION_DIR/TEST-3-FULL-PROMPT.md"
echo "Generated: TEST-3-FULL-PROMPT.md"

echo ""
echo "All prompts generated successfully!"
echo ""
echo "Prompt sizes:"
wc -l "$ITERATION_DIR"/TEST-*-FULL-PROMPT.md

echo ""
echo "Next steps:"
echo "1. Copy TEST-1-FULL-PROMPT.md content and submit to Grok"
echo "2. Save Grok's output to test-1-grok.cns"
echo "3. Validate with: ./cns-validate test-1-grok.cns"
echo "4. Test with: ./cns-run test-1-grok.cns /tmp/test-input.txt"
echo "5. Repeat for tests 2 and 3"
