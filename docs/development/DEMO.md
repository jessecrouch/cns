# CNS Webserver Demo

## Quick Start

```bash
# Start the webserver
./cns-run examples/demo-webserver.cns

# In another terminal, test it:
curl http://localhost:8080/

# Or open in browser:
# http://localhost:8080/
```

## Expected Output

The server will display:
```
✓ CNS Webserver started on port 8080
✓ Open http://localhost:8080/ in your browser
✓ Press Ctrl+C to stop
```

Each request shows:
```
✓ Served request #1
✓ Served request #2
...
```

## What You'll See

A styled HTML page with:
- **Purple gradient background**
- **White content box** with shadow
- **Dynamic connection counter** that increments with each refresh
- **Proper CSS styling** with fonts and colors
- **Responsive layout**

## Features Demonstrated

1. **TCP Socket Server** - Real network I/O
2. **HTTP Protocol** - Proper headers and response
3. **Multiline Strings** - HTML with escape sequences
4. **Variable Substitution** - Dynamic content (`{port}`, `{connection_count}`)
5. **CSS Escaping** - `\{` and `\}` for literal braces
6. **HTML Attributes** - Quoted attributes like `class="box"`
7. **Persistent State** - Connection counter across requests
8. **Narrative Flow** - Step-by-step execution with causality

## Code Structure

```cns
Story: Demo webserver serving HTML - runs until cancelled

Given:
  port: Integer = 8080
  connection_count: Integer = 0

Step 1 → Create server socket
Step 2 → Accept incoming connection
Step 3 → Read HTTP request
Step 4 → Send HTML response with CSS
Step 5 → Close client connection
Step 6 → Increment connection counter
Step 7 → Check if should continue (loops back to Step 2)

End: Close server and print stats
```

## Stopping the Server

Press `Ctrl+C` or run:
```bash
fuser -k 8080/tcp
```

## Troubleshooting

**Port already in use:**
```bash
fuser -k 8080/tcp  # Kill existing process
./cns-run examples/demo-webserver.cns
```

**Can't connect:**
- Check firewall settings
- Verify server is running (should show startup messages)
- Try `curl -v http://localhost:8080/` for verbose output

## Next Steps

Modify the HTML in Step 4 to:
- Change colors/styling
- Add more dynamic content
- Add JavaScript
- Create multiple pages

See `AGENTS.md` for development guidance.
