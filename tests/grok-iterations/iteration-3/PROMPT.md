# Grok Iteration 3: Request Logger with File Persistence

## Task Description

Build a web server in CNS that logs all incoming HTTP requests to a CSV file and provides an endpoint to view the request history.

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

## Expected Behavior

```bash
# First request
curl http://localhost:8080/
# Response: Request Logger Server

# Second request  
curl http://localhost:8080/
# Response: Request Logger Server

# Check history
curl http://localhost:8080/history
# Response: Shows 2 logged requests with timestamps
```

## CSV File Example

```csv
timestamp,method,path,client
2025-11-03 14:30:45,GET,/,client-1
2025-11-03 14:30:50,GET,/,client-2
```

## CNS Features to Use

- **HTTP Server**: Create socket server and accept connections
- **DateTime**: `NOW` function to get current timestamp
- **CSV Writing**: Append to CSV file with proper format
- **File I/O**: Check if file exists, read for history
- **String Formatting**: Build CSV lines properly
- **Conditional Logic**: Different behavior for `/` vs `/history`

## Success Criteria

1. Server starts on port 8080
2. Requests to `/` are logged to `requests.csv`
3. Requests to `/history` show all previous requests
4. CSV file has proper headers and format
5. Timestamps are human-readable
6. `/history` requests are NOT logged (to prevent clutter)

## Hints

- Use `Effect: Append to file requests.csv line "{csv_line}"` to add log entries
- Use `NOW` function for timestamps
- Check if path contains "history" to skip logging
- Format CSV line like: `"2025-11-03 14:30:45,GET,/,client-1"`
- Read entire file content and return it for history view

## Testing the Solution

```bash
# Start the server (will run indefinitely)
./cns-run grok-request-logger.cns &

# Test main endpoint
curl http://localhost:8080/
curl http://localhost:8080/
curl http://localhost:8080/

# View history
curl http://localhost:8080/history

# Check the CSV file directly
cat requests.csv

# Stop the server
pkill -f grok-request-logger
```

## Notes

- This builds on the multi-route webserver by adding persistence
- Tests CSV writing, datetime handling, and file operations
- Real-world use case: Basic request logging/analytics
- Should handle multiple requests cleanly
