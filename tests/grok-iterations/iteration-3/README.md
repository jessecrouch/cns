# Iteration 3: Request Logger with File Persistence

## Overview

This iteration tests Grok's ability to build a web server that:
- Logs all requests to a CSV file
- Uses datetime functions
- Provides a history viewing endpoint
- Handles file I/O operations

## Complexity Level

**Difficulty**: ⭐⭐ Moderate (slightly harder than multi-route webserver)

**New Skills Required**:
- CSV file writing
- DateTime operations (NOW function)
- File reading/appending
- Conditional logging (skip /history endpoint)

## Files

- `PROMPT.md` - The prompt to give to Grok
- `test-request-logger.sh` - Automated test script
- `EXPECTED-OUTPUT.md` - Expected behavior and output
- `grok-request-logger.cns` - Grok's solution (to be created)

## How to Run This Test

### Step 1: Give Prompt to Grok

Copy the contents of `PROMPT.md` and ask Grok to generate the CNS code.

### Step 2: Save Grok's Output

Save Grok's generated code as `grok-request-logger.cns` in this directory.

### Step 3: Start the Server

```bash
# From the cns root directory
./cns-run tests/grok-iterations/iteration-3/grok-request-logger.cns &
SERVER_PID=$!

# Give it a moment to start
sleep 2
```

### Step 4: Run the Tests

```bash
cd tests/grok-iterations/iteration-3
./test-request-logger.sh
```

### Step 5: Stop the Server

```bash
kill $SERVER_PID

# Or if you lost the PID:
pkill -f grok-request-logger
```

## Success Criteria

✅ **Pass**: Server logs requests correctly, history works, CSV format is valid, /history not logged

⚠️ **Partial**: Works but has minor issues (e.g., timestamp format not ideal, CSV formatting issues)

❌ **Fail**: Server crashes, CSV not created, logging doesn't work, or /history gets logged

## What This Tests

### Features Used
1. **HTTP Server** - Creating socket server, accepting connections
2. **DateTime** - Using NOW function for timestamps
3. **CSV Writing** - Proper CSV format with headers
4. **File I/O** - Reading and appending to files
5. **String Operations** - Building CSV lines
6. **Conditional Logic** - Different behavior for different routes

### LLM Challenges
- Understanding CSV format requirements
- Knowing to skip logging the /history endpoint
- Properly formatting datetime stamps
- Creating file with headers on first use
- Reading file contents for display

## Common Issues to Watch For

1. **Timestamp Format**: Should be human-readable, not epoch time
2. **CSV Headers**: First line should be column names
3. **Logging /history**: Should NOT log the history endpoint itself
4. **File Appending**: Should append, not overwrite
5. **CSV Formatting**: Proper commas, handle special characters

## Tips for Grok

If Grok struggles, provide these hints:
- "Use NOW function to get current timestamp"
- "Check if the path is /history to skip logging"
- "Append to file with: `Effect: Append to file requests.csv line '{data}'`"
- "Create header row if file doesn't exist"

## Results

Record Grok's performance:

**Date**: _______
**Grok Version**: _______
**Attempt**: ☐ 1st try  ☐ 2nd try  ☐ 3rd try
**Result**: ☐ Pass  ☐ Partial  ☐ Fail

**Notes**:
- 
- 
- 

**Code Quality** (1-5): ___
**Correctness** (1-5): ___
**CNS Idioms** (1-5): ___

## Next Steps

If Grok passes this test, move to:
- Iteration 4: Todo API (SQLite + JSON + CRUD operations)
- Or other moderate complexity projects
