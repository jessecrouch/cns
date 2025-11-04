# Test 2: Background Job Manager CLI

## Task

Build a job queue manager CLI tool in CNS that can:

1. **Launch background jobs** - Run shell commands in background
2. **List running jobs** - Show all active jobs with their PIDs and status
3. **Wait for job completion** - Wait for specific job to finish
4. **Kill jobs** - Stop running jobs by PID

The tool should accept a command as the first argument and additional arguments as needed.

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
# or
# Output: Job 12345: completed
# or
# Output: Job 12345: not-found
```

### 3. `wait <pid>`
Wait for a job to complete and show exit code.

```bash
./cns-run job-manager.cns wait 12345
# Output: Job 12345 completed with exit code: 0
# or
# Output: Job 12345 not found
```

### 4. `kill <pid>`
Kill a running job.

```bash
./cns-run job-manager.cns kill 12345
# Output: Job 12345 killed successfully
# or
# Output: Failed to kill job 12345
```

## Requirements

1. Use `ARGS[0]` to get the command (run/status/wait/kill)
2. Use `ARGS[1]` to get the job command or PID
3. Use `SHELL "command" BACKGROUND INTO pid` to launch background jobs
4. Use `STATUS OF pid` to check job status
5. Use `WAIT FOR pid` to wait for completion
6. Use `KILL pid` to terminate jobs
7. Handle invalid commands gracefully
8. Parse PID from string to integer using `PARSE_INT`

## Expected Logic Flow

### For "run" command:
1. Get command from ARGS[1]
2. Launch command in background using SHELL BACKGROUND
3. Print PID

### For "status" command:
1. Get PID from ARGS[1]
2. Parse PID to integer
3. Check STATUS OF pid
4. Print status

### For "wait" command:
1. Get PID from ARGS[1]
2. Parse PID to integer
3. WAIT FOR pid (blocking)
4. Print exit code

### For "kill" command:
1. Get PID from ARGS[1]
2. Parse PID to integer
3. KILL pid
4. Print result

## Example Usage Session

```bash
# Start a long-running job
./cns-run job-manager.cns run "sleep 30"
# Output: Job started: PID 12345

# Check status
./cns-run job-manager.cns status 12345
# Output: Job 12345: running

# Kill it
./cns-run job-manager.cns kill 12345
# Output: Job 12345 killed successfully

# Check status again
./cns-run job-manager.cns status 12345
# Output: Job 12345: not-found
```

## CNS Features to Use

- **CLI Arguments**: `ARGS[0]`, `ARGS[1]`
- **Process Management**: `SHELL BACKGROUND`, `STATUS OF`, `WAIT FOR`, `KILL`
- **Type Conversion**: `PARSE_INT` to convert PID string to integer
- **Conditionals**: Route to different handlers based on command
- **Error Handling**: Handle invalid commands and PIDs

## Success Criteria

- [ ] Code validates with `./cns-validate`
- [ ] "run" command launches background jobs and returns PID
- [ ] "status" command checks job status correctly
- [ ] "wait" command blocks until job completes
- [ ] "kill" command terminates jobs
- [ ] Invalid commands show error message
- [ ] PID parsing works correctly

---

**Generate a complete CNS program that solves this task.**

Use the CNS syntax reference below:

[INCLUDE FULL SYNTAX.MD HERE]
