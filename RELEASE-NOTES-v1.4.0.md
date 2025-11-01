# CNS v1.4.0 Release Notes

**Release Date:** November 1, 2025  
**Focus:** String Manipulation & CSV Support - Complete Phase B

## Overview

Version 1.4.0 completes Phase B of the CNS development roadmap by adding comprehensive string manipulation helpers and CSV file support. This release marks **100% completion of Phase B**, providing CNS with a complete toolkit for web backend development and data processing.

---

## New Features

### 1. String Helper Functions

Six new string operations for text manipulation:

**TRIM** - Remove leading and trailing whitespace:
```cns
Then: cleaned becomes TRIM "  hello  "
// Result: "hello"
```

**UPPERCASE** - Convert to uppercase:
```cns
Then: upper becomes UPPERCASE "hello world"
// Result: "HELLO WORLD"
```

**LOWERCASE** - Convert to lowercase:
```cns
Then: lower becomes LOWERCASE "HELLO WORLD"
// Result: "hello world"
```

**REPLACE** - Find and replace text:
```cns
Then: result becomes REPLACE "world" WITH "CNS" IN "hello world"
// Result: "hello CNS"
```

**JOIN** - Combine list elements with delimiter:
```cns
Given:
  items: List = ["apple", "banana", "cherry"]
Then: text becomes JOIN items WITH ", "
// Result: "apple, banana, cherry"
```

**LENGTH_OF** - Get length of string or list:
```cns
Then: len1 becomes LENGTH_OF "hello"     // Returns: 5
Then: len2 becomes LENGTH_OF ["a", "b"]  // Returns: 2
```

### 2. CSV File Support

Read and write CSV files with header support:

**CSV WRITE** - Write data to CSV file:
```cns
Given:
  headers: List = ["name", "age", "city"]
  data: List = []

Step 1 → Build data
  Effect: ADD ["Alice", "30", "NYC"] TO LIST data
  Effect: ADD ["Bob", "25", "LA"] TO LIST data

Step 2 → Save to file
  Effect: CSV WRITE data TO "report.csv" WITH HEADERS headers
```

**CSV READ** - Read CSV file into list of maps:
```cns
Step 3 → Load from file
  Then: loaded becomes CSV READ "report.csv"
  Then: count becomes LENGTH_OF loaded
  // Returns list of hash-tables, one per row
  // Each table has header names as keys
```

### 3. List Literal Parsing Fix

Fixed bug where list literals like `[1, 2, 3]` in Given sections were treated as strings instead of actual lists. Now lists are properly parsed and can be used with all list operations.

**Before (broken):**
```cns
Given:
  numbers: List = [10, 20, 30]
// Was stored as string "[10, 20, 30]"
```

**After (fixed):**
```cns
Given:
  numbers: List = [10, 20, 30]
// Now properly stored as list (10 20 30)
```

---

## Technical Implementation

### String Operations (src/cns.lisp:1776-1843)
- All string helpers implemented as eval-expr operations
- TRIM uses built-in Common Lisp trim function
- UPPERCASE/LOWERCASE use string-upcase/string-downcase
- REPLACE uses existing replace-all helper
- JOIN uses loop-based concatenation
- LENGTH_OF works with both strings and lists

### CSV Operations (src/cns.lisp:1846-1872, 2540-2575)
- CSV READ: Returns list of hash-tables with header keys
- CSV WRITE: Accepts list of lists or list of hash-tables
- Uses simple comma delimiter (no quote escaping yet)
- Header row always included

### List Parsing Fix (src/cns.lisp:2763-2778)
- Detects `[...]` pattern in Given initialization
- Calls eval-expr to properly parse list literals
- Preserves backward compatibility with quoted strings

---

## Examples

New comprehensive examples added:

1. **examples/test-string-helpers.cns** (8 steps)
   - Demonstrates all string operations
   - Tests TRIM, UPPERCASE, LOWERCASE, REPLACE
   - Tests JOIN and LENGTH_OF with lists

2. **examples/test-csv.cns** (4 steps)
   - Creates sample data with multiple rows
   - Writes CSV file with headers
   - Reads file back and verifies count

3. **examples/csv-report.cns** (3 steps)
   - Business report example
   - Builds sales data, writes CSV, reads back
   - Demonstrates practical CSV workflow

---

## Breaking Changes

None. All changes are backward compatible.

---

## Bug Fixes

1. **List Literal Parsing** - Fixed Given: section not parsing list literals correctly
2. **JOIN Format** - Fixed FORMAT directive for proper delimiter insertion
3. **LIST Operations** - Verified ADD TO LIST works with empty list initialization

---

## Phase B Completion Status

**Phase B: 100% Complete** ✅

Completed features:
- ✅ HTTPS Support (v1.1.0)
- ✅ Enhanced JSON Parser (v1.1.0)
- ✅ Environment Variables (v1.1.0)
- ✅ Regular Expressions (v1.2.0)
- ✅ Date/Time Operations (v1.2.0)
- ✅ Database Support (v1.3.0)
- ✅ String Helpers (v1.4.0) - **NEW**
- ✅ CSV Support (v1.4.0) - **NEW**

---

## Testing

All 69 examples validate successfully:
- Original 66 examples
- New: test-string-helpers.cns
- New: test-csv.cns
- New: csv-report.cns

---

## What's Next

**Phase C Options:**

1. **Benchmark Track** - Prerequisites for SWE-Bench:
   - Shell execution with stdout/stderr capture
   - Git operations (clone, checkout, diff, status)
   - Diff generation for patch files

2. **Advanced Features** - Extended capabilities:
   - XML/HTML parsing
   - Advanced HTTP (cookies, sessions, auth)
   - Crypto operations (hashing, encryption)

3. **Tooling & Polish** - Developer experience:
   - Better error messages
   - Debugger/stepper
   - Language server protocol (LSP)

---

## Upgrade Instructions

No changes required. Simply use the new operations in your CNS code.

```bash
# Test string helpers
./cns-run examples/test-string-helpers.cns

# Test CSV operations
./cns-run examples/csv-report.cns
```

---

## Contributors

CNS Language Development Team

---

## Links

- **GitHub:** [CNS Repository]
- **Documentation:** See examples/ directory
- **Previous Release:** [v1.3.0 - Database Support](RELEASE-NOTES-v1.3.0.md)
