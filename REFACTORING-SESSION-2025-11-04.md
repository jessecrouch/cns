# Expression Parser Refactoring Session - November 4, 2025

## Summary

Successfully refactored the CNS expression parser (`eval-expr`) and **fixed the FILE EXISTS truncation bug** that was discovered in the previous session.

## Problem

The FILE EXISTS operation was failing when used with literal string paths:
- `FILE EXISTS path_var` ✅ worked
- `FILE EXISTS "/tmp/file.txt"` ❌ failed (truncated to empty string)

Root cause: The 606-line monolithic `eval-expr` function had implicit precedence. The `/` division operator (line 4249) was intercepting `FILE EXISTS "/tmp"` before the FILE EXISTS parser (line 4357) could handle it.

## Solution

Applied the same refactoring pattern that worked for effects (Nov 3):

### Phase 1: Extract Parser Functions
Created `can-parse-X-p` / `try-X` function pairs for:
1. **Literals**: String, filepath, boolean  
2. **CLI/Environment**: ENV, ARG, HAS_FLAG, ARGS
3. **Variables**: Variable lookup, list literals, assignment
4. **Date/Time**: NOW, TIMESTAMP

### Phase 2: Establish Explicit Precedence
Reorganized `eval-expr` with clear precedence hierarchy:
```lisp
1. Literals (strings, paths, booleans)
2. CLI/Environment operations  
3. Date/Time operations
4. Variables and lists
5. File system operations ← MOVED HERE (was after operators)
6. Math functions
7. Comparison operators
8. Arithmetic operators
```

**Key Fix**: Moved FILE EXISTS and READ FILE checks **before** arithmetic operators. This prevents `/` from splitting paths like `/tmp/file.txt`.

### Phase 3: Add Debug Support
- Added `*eval-expr-debug*` flag for future debugging
- Can be enabled to trace which parser matches each expression

## Results

- ✅ All 38 tests passing (100%)
- ✅ FILE EXISTS works with literal paths: `FILE EXISTS "/tmp"` → `T`
- ✅ No regressions in existing functionality
- ✅ Cleaner, more maintainable code structure

## Testing

Direct SBCL test confirmed the fix:
```common-lisp
Test 1: FILE EXISTS with variable
Result: T

Test 2: FILE EXISTS with literal  
Result: T  ← FIXED!
```

## Code Changes

**File**: `src/cns.lisp`
- +332 lines (new helper functions)
- -184 lines (removed inline code)
- Net: +148 lines (but much more organized)

**New helper functions** (selected):
- `can-parse-string-literal-p` / `try-string-literal`
- `can-parse-filepath-literal-p` / `try-filepath-literal`
- `can-parse-env-p` / `try-env`
- `can-parse-file-exists-p` / `try-file-exists` (moved earlier)

## Architecture Insight

The refactoring revealed an important pattern:
- **Effects system** (Nov 3): Clean after refactoring
- **Expression system** (today): Clean after refactoring  
- Both use `can-parse-X-p` / `try-X` pattern for explicit precedence

This confirms that **monolithic parser functions hide bugs**, while **specialized parsers with explicit precedence** make bugs obvious early.

## What's Next

The expression parser is now well-organized, but there's still room for improvement:
- Extract remaining inline parsers (JSON, list operations, property access)
- Add more comprehensive FILE EXISTS tests to prevent regressions
- Consider adding a test specifically for literal paths in file operations

## Lessons Learned

1. **Implicit precedence is dangerous**: The order of `cond` clauses matters, but it's not obvious in a 600-line function
2. **Test-driven refactoring works**: Running tests after each extraction caught issues immediately
3. **Pattern reuse is powerful**: Applying the effect system pattern to expressions was straightforward
4. **Bugs reveal architecture problems**: The FILE EXISTS bug exposed the need for this refactoring

## Time Investment

- Planning: 30 minutes (reviewing refactoring plan from previous session)
- Execution: 2.5 hours (extracting parsers, fixing bug, testing)
- Total: 3 hours

Worth it! The bug is fixed and the code is maintainable.

---

Session completed: November 4, 2025
Commit: `281ae14` - "Refactor eval-expr: extract literal/CLI/variable parsers and fix FILE EXISTS bug"
