# Testing the Request Logger

## Manual Testing Instructions

### Start the Server

```bash
cd /path/to/cns
rm -f requests.csv  # Clean up old logs
./cns-run tests/grok-iterations/iteration-3/reference-request-logger.cns
```

You should see:
```
Request Logger Server started on port 8080
```

### Test in Another Terminal

```bash
# Test 1: Main endpoint
curl http://localhost:8080/
# Expected: "Request Logger Server"

# Test 2: Another request
curl http://localhost:8080/
# Expected: "Request Logger Server"

# Test 3: View history
curl http://localhost:8080/history
# Expected: CSV content with 2 logged requests

# Test 4: Check CSV file directly
cat requests.csv
# Expected: 2 lines like:
# 2025-11-03 13:42:39,GET,/,client-1
# 2025-11-03 13:42:40,GET,/,client-2
```

### Stop the Server

Press Ctrl+C in the server terminal, or:
```bash
pkill -f reference-request-logger
```

## What the Reference Implementation Does

✅ **Works:**
- Creates HTTP server on port 8080
- Logs GET / requests to requests.csv
- Shows request history at GET /history
- Does NOT log /history requests (prevents clutter)
- Uses TIMESTAMP() for datetime stamps
- Appends to CSV file (doesn't overwrite)

⚠️ **Known Limitations:**
- CSV file doesn't have header row (minor)
- First request might timeout due to CNS startup time
- Server needs manual stop (Ctrl+C)

## Reference Implementation Features

1. **HTTP Server** ✅
   - Socket creation
   - Connection handling
   - Request parsing

2. **CSV Logging** ✅
   - Append to file
   - Timestamp formatting
   - Client tracking

3. **Datetime** ✅
   - TIMESTAMP() function
   - Human-readable format

4. **File Operations** ✅
   - READ FROM FILE
   - Append to file
   - File existence checking (implicit)

5. **Conditional Logic** ✅
   - Different routes
   - Skip logging for /history

## For Grok Testing

The reference implementation demonstrates that this task is achievable in CNS. When testing Grok:

- Grok should produce similar structure
- May have different variable names
- Might add CSV headers (good!)
- Could have better error handling
- Style may vary but functionality should match

## Test Success Criteria

- [ ] Server starts without errors
- [ ] GET / returns "Request Logger Server" (or similar message)
- [ ] Requests are logged to requests.csv
- [ ] CSV has format: timestamp,method,path,client
- [ ] GET /history displays logged requests  
- [ ] GET /history is NOT logged in CSV
- [ ] Multiple requests work correctly
- [ ] Timestamps are human-readable
