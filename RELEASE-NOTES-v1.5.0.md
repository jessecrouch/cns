# CNS v1.5.0 Release Notes

**Release Date:** November 1, 2024  
**Phase:** C - Benchmark Track (Initial Release)  
**Focus:** Shell Execution & Git Operations for SWE-Bench Preparation

## Overview

CNS v1.5.0 marks the beginning of **Phase C: Benchmark Track**, introducing powerful shell execution and git operations capabilities. This release prepares CNS for sophisticated software engineering tasks and benchmark evaluation (SWE-Bench).

## New Features

### 1. Shell Command Execution

Execute arbitrary shell commands with full control over stdout, stderr, and exit codes.

**Syntax:**
```cns
SHELL "command" INTO output WITH EXIT_CODE code
SHELL "command" INTO output WITH EXIT_CODE code AND ERROR error
```

**Features:**
- Execute any shell command
- Capture stdout to a variable
- Capture exit code (Number)
- Optionally capture stderr
- Support for pipes, redirection, environment variables

**Example:**
```cns
Effect: SHELL "ls -la" INTO listing WITH EXIT_CODE status

If: status == 0
Then: PRINT "Files:"
      PRINT listing
```

### 2. Git Operations

Native git command support for version control operations.

**Syntax:**
```cns
GIT STATUS INTO output WITH EXIT_CODE code
GIT DIFF INTO patch WITH EXIT_CODE code
GIT CHECKOUT "branch" WITH STATUS result
GIT ADD "files" WITH STATUS result
GIT COMMIT "message" WITH STATUS result
GIT CLONE "url" INTO "directory" WITH STATUS result
```

**Features:**
- Check repository status
- View diffs
- Switch branches
- Stage files
- Create commits
- Clone repositories

**Example:**
```cns
Effect: GIT STATUS INTO status WITH EXIT_CODE code
Then: PRINT "Repository status:"
      PRINT status

Effect: GIT DIFF INTO changes WITH EXIT_CODE code
Then: PRINT "Uncommitted changes:"
      PRINT changes
```

## Examples

Four new comprehensive examples demonstrate the new capabilities:

1. **shell-demo.cns** - Simple shell execution demo
2. **test-shell.cns** - Comprehensive shell command testing
3. **test-git-basic.cns** - Basic git operations
4. **test-git-workflow.cns** - Complete git workflow automation

## Technical Details

### Implementation

- Uses `sb-ext:run-program` with `/bin/sh` for shell execution
- Uses `/usr/bin/git` for git operations
- Graceful error handling with exit codes and stderr capture
- Full stdout/stderr stream capture
- Non-blocking process execution with `:wait t`

### File Changes

**Modified:**
- `src/cns.lisp` (lines 2669-2980) - Added SHELL and GIT effect handlers

**Added:**
- `examples/shell-demo.cns`
- `examples/test-shell.cns`
- `examples/test-git-basic.cns`
- `examples/test-git-workflow.cns`
- `RELEASE-NOTES-v1.5.0.md`

## Validation

All 59 .cns examples pass validation:
- 55 existing examples continue to work
- 4 new shell/git examples validated
- No breaking changes to existing functionality

## Roadmap Impact

### Phase C Progress: 30% Complete

**Completed (v1.5.0):**
1. ✅ Shell execution with output capture
2. ✅ Basic Git operations (status, diff, checkout, add, commit, clone)

**Remaining for Phase C:**
3. Advanced diff generation for patch creation
4. Git branch management and merging
5. Complete SWE-Bench agent implementation
6. Benchmark evaluation and optimization

## Use Cases

### Software Engineering Automation
```cns
# Check test status
Effect: SHELL "pytest tests/" INTO output WITH EXIT_CODE status
If: status == 0
Then: PRINT "All tests passed"
```

### Repository Management
```cns
# Automated commit workflow
Effect: GIT ADD "src/*.lisp" WITH STATUS result
Effect: GIT COMMIT "feat: add new feature" WITH STATUS commit_result
Then: PRINT "Changes committed"
```

### Build Automation
```cns
# Build and test pipeline
Effect: SHELL "make clean && make" INTO build_output WITH EXIT_CODE build_status
If: build_status == 0
Then: Effect: SHELL "make test" INTO test_output WITH EXIT_CODE test_status
```

## Breaking Changes

None. This release is fully backward compatible with v1.4.0.

## Dependencies

- SBCL (Steel Bank Common Lisp)
- `/bin/sh` (standard shell)
- `/usr/bin/git` (for git operations)

## Future Enhancements (v1.5.x)

Planned for upcoming minor releases:

1. **Git Advanced Operations**
   - Branch creation/deletion
   - Merge operations
   - Conflict resolution
   - Tag management

2. **Enhanced Shell Features**
   - Working directory control
   - Environment variable management
   - Process backgrounding
   - Signal handling

3. **SWE-Bench Integration**
   - Issue analysis
   - Patch generation
   - Test execution
   - Verification workflows

## Migration Guide

No migration needed. Simply update to v1.5.0 and start using the new SHELL and GIT commands.

## Known Issues

None reported.

## Contributors

- CNS Development Team

## License

Same as CNS project license.

---

**Next Release:** v1.5.1 (Expected: November 2024)  
**Focus:** Advanced git operations and diff generation
