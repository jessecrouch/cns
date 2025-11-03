# Expected Output: Request Logger

## When running the server

```bash
$ ./cns-run grok-request-logger.cns
Server started on port 8080
Waiting for connections...
```

## When making requests

### Request 1: GET /
```bash
$ curl http://localhost:8080/
Request Logger Server
```

### Request 2: GET /
```bash
$ curl http://localhost:8080/
Request Logger Server
```

### Request 3: GET /history
```bash
$ curl http://localhost:8080/history
=== Request History ===

timestamp,method,path,client
2025-11-03 14:30:45,GET,/,client-1
2025-11-03 14:30:50,GET,/,client-2

Total requests logged: 2
```

## CSV File Contents (requests.csv)

```csv
timestamp,method,path,client
2025-11-03 14:30:45,GET,/,client-1
2025-11-03 14:30:50,GET,/,client-2
```

## Key Behaviors

1. **Each GET / request**:
   - Returns "Request Logger Server"
   - Appends entry to requests.csv with current timestamp
   
2. **GET /history request**:
   - Reads requests.csv
   - Displays formatted history
   - Does NOT log itself (important!)

3. **CSV Format**:
   - Header row: `timestamp,method,path,client`
   - Data rows: `2025-11-03 14:30:45,GET,/,client-1`
   - Proper CSV formatting (quotes if values contain commas)

4. **Error Handling**:
   - If requests.csv doesn't exist on first request, create it with headers
   - If /history called before any requests logged, show empty history

## Validation Checklist

- [ ] Server starts successfully on port 8080
- [ ] GET / returns correct message
- [ ] Each request is logged with timestamp
- [ ] Timestamps are in human-readable format (not epoch)
- [ ] CSV has proper header row
- [ ] GET /history displays logged requests
- [ ] GET /history is NOT logged in the CSV
- [ ] Multiple requests work correctly
- [ ] CSV file persists between requests
- [ ] File is appended to (not overwritten)
