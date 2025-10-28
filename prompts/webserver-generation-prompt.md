# Webserver Generation Prompt for CNS

## Quick Webserver Generation Template

Use this prompt template to generate webserver CNS code from natural language descriptions.

---

## System Message (for LLM)

You are generating code in **Causal Narrative Script (CNS)**, a language optimized for LLM comprehension.

### CNS Webserver Pattern:

```
Story: [What the server does]

Given:
  port: Integer = [port_number]
  server_socket: Socket
  [other variables as needed]

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for HTTP connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client request

Step 3 → Read request
  Effect: Network read
  Because: Get client HTTP data

Step 4 → [Process/route request]
  Because: [why]
  [routing logic if needed]

Step N → Send response
  Effect: Send "[HTTP response]" to client
  Because: Return data to client

Step N+1 → If [keep serving], repeat from Step 2
  Because: Handle multiple connections
  Otherwise: go to End

End: Return "done"
```

**Key Rules:**
1. Always declare `Effect:` for socket operations
2. Use `repeat from Step N` for loops
3. Include `Because:` for every step
4. Declare all variables in `Given:`

---

## Example Prompts

### Prompt 1: Basic Server
**User:** "Create a webserver on port 3000"

**Expected CNS:**
```cns
Story: Simple webserver on port 3000

Given:
  port: Integer = 3000
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Listen for connections

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle client

Step 3 → Send response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nHello from port 3000" to client
  Because: Respond to request

Step 4 → If TRUE, repeat from Step 2
  Because: Keep serving

End: Return "done"
```

### Prompt 2: Multi-Route Server
**User:** "Create a webserver with routes for / and /about"

**Expected CNS:**
```cns
Story: Webserver with multiple routes

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle request
  Then: request becomes parse HTTP request from request_data

Step 3 → If request.url = "/", go to Step 4
  Because: Route to home page
  Otherwise: go to Step 5

Step 4 → Send home response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nHome Page" to client
  Because: Return home content
  Then: go to Step 6

Step 5 → Send about response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nAbout Page" to client
  Because: Return about content

Step 6 → If TRUE, repeat from Step 2
  Because: Continue serving

End: Return "done"
```

### Prompt 3: Server with Error Handling
**User:** "Create a webserver that handles 404 errors"

**Expected CNS:**
```cns
Story: Webserver with 404 handling

Given:
  port: Integer = 8080
  server_socket: Socket

Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server

Step 2 → Accept connection
  Effect: Accept connection on server_socket
  Because: Handle request
  Then: request becomes parse HTTP request from request_data

Step 3 → If request.url = "/", go to Step 4
  Because: Valid route
  Otherwise: go to Step 5

Step 4 → Send success response
  Effect: Send "HTTP/1.1 200 OK\r\n\r\nSuccess" to client
  Because: Return content
  Then: go to Step 6

Step 5 → Send 404 response
  Effect: Send "HTTP/1.1 404 Not Found\r\n\r\nPage not found" to client
  Because: Handle invalid route

Step 6 → If TRUE, repeat from Step 2
  Because: Keep serving

Error:
  Return "Server crashed"
  Effect: Log "Server error"
  Because: Handle unexpected failure

End: Return "done"
```

---

## Testing Your Generated Code

After generating CNS code:

1. **Validate Structure:**
   ```bash
   sbcl --eval "(load \"cns.lisp\")" \
        --eval "(validate-cns (parse-cns \"[your-code]\"))" \
        --quit
   ```

2. **Run the Server:**
   ```bash
   # Save to file
   echo "[your-code]" > test-server.cns
   ./cns-run test-server.cns
   ```

3. **Test with curl:**
   ```bash
   curl http://localhost:8080/
   ```

---

## Common Mistakes to Avoid

❌ **Missing Effect declarations:**
```cns
Step 1 → Create socket  # WRONG - no Effect:
```

✅ **Correct:**
```cns
Step 1 → Create server_socket on port
  Effect: Create socket server_socket on port
  Because: Start server
```

❌ **Undefined variables:**
```cns
Step 2 → Use server_socket  # WRONG - not declared in Given:
```

✅ **Correct:**
```cns
Given:
  server_socket: Socket  # Declared first

Step 2 → Accept connection on server_socket
```

❌ **Missing Because:**
```cns
Step 1 → Do something  # WRONG - no Because:
```

✅ **Correct:**
```cns
Step 1 → Do something
  Because: Explanation of why
```

---

## Quick Reference: HTTP Responses

### Success Response
```
"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html>content</html>"
```

### JSON Response
```
"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"status\": \"ok\"}"
```

### 404 Response
```
"HTTP/1.1 404 Not Found\r\n\r\nPage not found"
```

### 500 Error
```
"HTTP/1.1 500 Server Error\r\n\r\nInternal error"
```

---

## Generation Prompt Template

Use this template when prompting an LLM:

```
Generate CNS code for the following task:

TASK: [description]

Requirements:
- Use CNS syntax (Story, Given, Step, Effect, Because, End)
- Declare all socket effects explicitly
- Include proper HTTP responses
- Handle errors if needed
- Use clear step descriptions

Output only valid CNS code.
```
