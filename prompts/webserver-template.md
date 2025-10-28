# CNS Webserver Generation Prompt Template

## System Prompt

You are an expert at writing CNS (Causal Narrative Script) code. CNS is a programming language designed for clarity and explicit reasoning, where every step must explain its purpose with a "Because:" clause.

## CNS Language Rules

### Structure
1. Start with `Story: <description>`
2. Declare all variables in `Given:` section with types and initial values
3. Write steps with `Step N →` followed by action description
4. Every step must have `Because:` explaining the reasoning
5. Use `Effect:` for side effects (I/O, network, etc.)
6. Use `Then:` for state changes
7. Use `If/Then/Otherwise` for conditionals
8. End with `End:` section for cleanup and return value
9. Optional `Error:` section for error handling

### Variable Declaration Format
```
Given:
  <name>: <Type> [<semantic-tag>] = <value>
```

Example:
```
Given:
  port: Integer [network port] = 8080
  socket: Socket [network listener]
  count: Integer [connection counter] = 0
```

### Step Format
```
Step N → <action description>
  Because: <reasoning>
  Effect: <side effect>  (optional)
  Then: <state change>   (optional)
```

### Conditional Format
```
Step N → If <condition>
  Then: <action if true>
  Otherwise: <action if false>
  Because: <reasoning>
```

### Available Types
- Integer: Whole numbers
- String: Text in quotes
- List: Arrays like [1, 2, 3]
- Socket: Network connection
- Object: Complex data structures

### Available Effects
- `Print "<message>"` - Output with {var} interpolation
- `Write "<content>" to <file>` - File writing
- `Append "<content>" to <file>` - File appending
- `Create socket <name> on <port>` - Socket creation
- `Accept connection` - Wait for client
- `Send "<data>" to <target>` - Network send
- `Network read` - Read from network
- `Network write` - Write to network
- `Close socket <name>` - Socket cleanup
- `Log "<message>"` - Error logging

### Control Flow
- `repeat from Step N` - Loop back
- `go to Step N` - Jump forward
- `go to End` - Exit early

## Generation Task

Given this task: **{TASK}**

Generate complete CNS code that:
1. Defines a clear Story describing the goal
2. Declares all necessary variables in Given section
3. Implements the logic in numbered Steps
4. Includes Because clauses for every step
5. Uses appropriate Effects for I/O operations
6. Handles errors with an Error block
7. Ends with proper cleanup

## Example: Simple Webserver

Task: "Create a webserver on port 8080 that responds with 'Hello, World!'"

```cns
Story: Run a simple webserver on port 8080

Given:
  port: Integer [network port] = 8080
  server_socket: Socket [network listener]
  response: String = "Hello, World!" [default response]
  connection_count: Integer = 0 [track connections]
  
Step 1 → Create server_socket on port
  Effect: Create socket server_socket on 8080
  Because: We need to listen for incoming HTTP connections
  
Step 2 → Accept connection on server_socket
  Effect: Accept connection
  Because: Wait for client request
  Then: connection_count becomes connection_count + 1
  
Step 3 → Parse HTTP request
  Effect: Network read
  Because: Receive and understand client's HTTP request
  
Step 4 → Send response to client
  Effect: Send "HTTP/1.1 200 OK\nContent-Type: text/plain\n\n{response}" to client
  Because: Return the greeting to the client
  
Step 5 → If connection_count < 3
  Then: repeat from Step 2
  Otherwise: go to End
  Because: Handle up to 3 connections for this demo
  
Error:
  Return "Server error"
  Effect: Log "Error in webserver"
  Because: Handle unexpected failures gracefully
  
End: Close server_socket
  Effect: Close socket server_socket
  Because: Clean up network resources properly
```

## Your Task

Task: {TASK}

Generate CNS code following the structure and rules above. Output ONLY the CNS code, no explanations.
