# Test 3: Task Runner REST API

## Task

Build a REST API server in CNS that accepts task submissions, runs them as background jobs, stores results in a database, and provides endpoints to check task status.

## Endpoints

### 1. `POST /tasks` - Submit a new task
**Request body (JSON):**
```json
{
  "command": "sleep 5 && echo done"
}
```

**Response (JSON-like):**
```
{"task_id": 1, "status": "running", "pid": 12345}
```

### 2. `GET /tasks/<id>` - Get task status
**Response:**
```
{"task_id": 1, "status": "running", "command": "sleep 5 && echo done", "pid": 12345}
```
or when completed:
```
{"task_id": 1, "status": "completed", "command": "sleep 5 && echo done", "exit_code": 0}
```

### 3. `GET /tasks` - List all tasks
**Response:**
```
[{"task_id": 1, "status": "completed", "exit_code": 0}, {"task_id": 2, "status": "running", "pid": 12346}]
```

### 4. `DELETE /tasks/<id>` - Kill a running task
**Response:**
```
{"task_id": 1, "status": "killed"}
```

## Requirements

1. **HTTP Server** on port 8080
2. **SQLite Database** (`tasks.db`) with schema:
   ```sql
   CREATE TABLE tasks (
     id INTEGER PRIMARY KEY,
     command TEXT,
     pid INTEGER,
     status TEXT,
     exit_code INTEGER
   )
   ```
3. **Background job execution** using SHELL BACKGROUND
4. **Process management** using STATUS OF and KILL
5. **JSON parsing** for POST body
6. **JSON-like responses** (can use string concatenation)

## Expected Logic Flow

### POST /tasks:
1. Parse JSON body to get command
2. Launch command as SHELL BACKGROUND
3. Store task in database (INSERT)
4. Return task_id, status="running", and PID

### GET /tasks/<id>:
1. Extract ID from path (e.g., "/tasks/1" → id=1)
2. Query database for task
3. If status="running": Check STATUS OF pid, update if completed
4. Return task details as JSON-like string

### GET /tasks:
1. Query all tasks from database
2. For each running task: check STATUS OF pid and update
3. Return list as JSON-like string

### DELETE /tasks/<id>:
1. Extract ID from path
2. Query database for task and get PID
3. KILL pid
4. Update database status="killed"
5. Return confirmation

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
# Start server
./cns-run task-runner.cns &

# Submit a task
curl -X POST http://localhost:8080/tasks -d '{"command":"sleep 10"}'
# Response: {"task_id": 1, "status": "running", "pid": 12345}

# Check task status
curl http://localhost:8080/tasks/1
# Response: {"task_id": 1, "status": "running", "command": "sleep 10", "pid": 12345}

# List all tasks
curl http://localhost:8080/tasks
# Response: [{"task_id": 1, "status": "running", "pid": 12345}]

# Kill task
curl -X DELETE http://localhost:8080/tasks/1
# Response: {"task_id": 1, "status": "killed"}

# Check status after killing
curl http://localhost:8080/tasks/1
# Response: {"task_id": 1, "status": "killed", "command": "sleep 10"}
```

## CNS Features to Use

- **HTTP Server**: Create socket, Accept connection, Network read, Send response
- **SQLite**: DATABASE CONNECT, DATABASE EXECUTE, DATABASE QUERY
- **Process Management**: SHELL BACKGROUND, STATUS OF, KILL, WAIT FOR
- **JSON**: PARSE JSON to extract command from POST body
- **String Operations**: Build JSON responses, parse URL paths
- **Control Flow**: Route based on REQUEST_METHOD and REQUEST_PATH

## Success Criteria

- [ ] Code validates with `./cns-validate`
- [ ] Server starts on port 8080
- [ ] POST /tasks submits jobs and returns task_id
- [ ] GET /tasks/<id> returns task status
- [ ] GET /tasks lists all tasks
- [ ] DELETE /tasks/<id> kills running tasks
- [ ] Database stores task information correctly
- [ ] Background jobs run independently
- [ ] Status updates reflect actual process state

## Hints

- Use string operations to extract task ID from path: `/tasks/1` → `1`
- For JSON responses, string concatenation is fine: `"{\"task_id\":" + id + "}"`
- Initialize database on server start with CREATE TABLE IF NOT EXISTS
- Use PARSE_INT to convert task_id from string to integer
- Check if STATUS OF returns "completed" and update database accordingly

---

**Generate a complete CNS program that solves this task.**

Use the CNS syntax reference below:

[INCLUDE FULL SYNTAX.MD HERE]
