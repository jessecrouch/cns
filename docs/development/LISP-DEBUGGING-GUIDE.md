# Lisp Debugging Guide for CNS Development

## Overview

This guide documents techniques for debugging Common Lisp syntax errors, particularly parentheses balance issues, based on real debugging sessions during CNS development.

## The Parentheses Problem

### Symptom
```
READ error during LOAD:
  end of file on #<SB-INT:FORM-TRACKING-STREAM>
  (in form starting at line: 3494, column: 0, position: 179778)
```

This error means: **There's an unclosed form (missing closing parenthesis) somewhere before line 3494.**

### Why It's Hard

**Simple paren counting doesn't work!** Lisp has:
- String literals with parens inside: `"(example)"`
- Comments with parens: `; (this is ignored)`
- Character literals: `#\(`

**You need to trace STRUCTURAL nesting, not just count characters.**

## The Methodology That Works

### Step 1: Isolate the Problem Section

Binary search to find which section has the issue:

```bash
# Test loading partial files
head -3500 src/cns.lisp > /tmp/test.lisp
echo "(print 'done)" >> /tmp/test.lisp
sbcl --non-interactive --load /tmp/test.lisp 2>&1 | grep "done"

# If error before 3500, try 3250
# If success, try 3750
# Repeat until you find the ~50 line range
```

### Step 2: Count Net Opens/Closes Per Section

**DON'T** count total parens in the file.  
**DO** count net balance per structural section.

```python
# Python script to find unbalanced sections
with open('src/cns.lisp', 'r') as f:
    lines = f.readlines()

cumulative_balance = 0
for i in range(start_line, end_line):
    line = lines[i]
    open_count = line.count('(')
    close_count = line.count(')')
    cumulative_balance += (open_count - close_count)
    
    if cumulative_balance < 0:
        print(f"Line {i+1}: balance goes NEGATIVE! cumulative={cumulative_balance}")
        print(f"  Content: {line.rstrip()}")
        break
```

### Step 3: Trace Structural Nesting

For each closing paren, ask: **"What does THIS close?"**

Example from CNS If/Otherwise fix (line 3718):

```lisp
                          (incf pc)))))))
```

Count backward through the structure:
```
(incf pc)))))))
         ^     ^ closes (incf pc)
        ^      ^ closes (unless ...)
       ^       ^ closes (t ...) case of inner cond
      ^        ^ closes inner (cond ...)
     ^         ^ closes (if ...) false branch
    ^          ^ closes (let ((cond-expr ...
   ^           ^ closes outer cond if-node case
```

**Result:** This line needs **7 closing parens** (1 open + 7 close = net -6)

### Step 4: Compare to Known-Good Structure

If you're modifying existing code:

```bash
# Extract the same structural section from backup
sed -n '3670,3675p' src/cns.lisp.backup-before-fix

# Compare structure
diff -u backup.txt current.txt
```

Look for:
- Same nesting level?
- Same number of closing parens on equivalent lines?
- Did you accidentally remove a closing paren?

## Common Pitfalls

### Pitfall 1: "Backup Loads, So It Must Be Balanced"

**WRONG!** The backup file can have unbalanced paren counts but still load correctly:

```bash
$ python3 count_parens.py backup.lisp
Opening: 6416
Closing: 6414
Balance: +2
```

This is because:
- Comments and strings contain parens that don't count
- The Lisp reader is smarter than simple counting

**Lesson:** Don't trust raw paren counts. Test if it LOADS.

### Pitfall 2: "I'll Just Add/Remove Parens Until It Works"

**DANGEROUS!** This can:
- Make it load but change semantics
- Create subtle bugs
- Break unrelated code

**Lesson:** Understand the structure before changing parens.

### Pitfall 3: "The Edit Tool Removed Lines!"

When using edit tools, **carefully check what gets replaced**:

```lisp
# BAD: This might delete more than intended
oldString: "line A\nline B"  # If not exact, could match wrong section

# GOOD: Include unique context
oldString: "unique-comment\nline A\nline B\nunique-next-line"
```

**Lesson:** Always verify edits with `sed -n 'line1,line2p'` before/after.

### Pitfall 4: "Net Balance Zero = Correct"

**WRONG!** A section can have net zero balance but still be wrong:

```lisp
# This has net 0 but is broken
(if condition
  (action-true)))  ; <-- closes if AND something else (wrong!)
)                   ; <-- extra closing paren
```

**Lesson:** Trace WHAT each paren closes, not just the count.

## Real-World Case Study: If/Otherwise Fix

### The Problem

Replacing lines 3661-3674 (14 lines) with new Otherwise execution code (59 lines).

### The Journey

1. **Attempt 1:** Used Python to create multi-line string, inserted as single list item
   - **Result:** File structure broken (multi-line string in line array)
   - **Lesson:** `writelines()` expects list of strings with `\n`, not one big string

2. **Attempt 2:** Split multi-line string into list properly
   - **Result:** Still failed to load
   - **Lesson:** Paren count was off

3. **Attempt 3:** Counted parens, found imbalance
   - **Result:** Line 3718 had 6 closing parens but needed 7
   - **Lesson:** Compare net balance to original structure

4. **Attempt 4:** Added 7th closing paren
   - **Result:** SUCCESS! File loads and tests pass ✅

### The Fix

```bash
# Before (original line 3674)
                      (t (incf pc))))))
# Net: 2 opens, 7 closes = -5

# After (new line 3681 + 3718)
# Line 3681:
                      (t
# Line 3718:  
                          (incf pc)))))))
# Net: (1 + 1) opens, (0 + 7) closes = -5  ✅ MATCHES!
```

## Tools and Techniques

### Quick Validation

```bash
# Does it load?
sbcl --non-interactive --load src/cns.lisp 2>&1 | grep -i error

# Find the exact error location
sbcl --non-interactive --load src/cns.lisp 2>&1 | grep -A5 "READ error"
```

### Compare Changes

```bash
# Show exactly what changed
diff -u src/cns.lisp.backup src/cns.lisp | less

# Check specific line range
diff -u <(sed -n '3660,3720p' backup.lisp) \
        <(sed -n '3660,3720p' current.lisp)
```

### Python Paren Analyzer

```python
#!/usr/bin/env python3
"""
Analyze parentheses balance in Lisp file.
NOTE: This is a simple check - doesn't handle strings/comments properly!
Use only as a quick sanity check.
"""

def analyze_section(filepath, start_line, end_line):
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    cumulative = 0
    for i in range(start_line - 1, end_line):
        line = lines[i]
        open_count = line.count('(')
        close_count = line.count(')')
        cumulative += (open_count - close_count)
        
        if cumulative < 0:
            print(f"⚠️  Line {i+1}: cumulative balance = {cumulative}")
            print(f"    {line.rstrip()}")
            print(f"    Opens: {open_count}, Closes: {close_count}")
            return False
    
    print(f"✅ Lines {start_line}-{end_line}: Final balance = {cumulative}")
    return cumulative == 0

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("Usage: analyze_parens.py FILE START_LINE END_LINE")
        sys.exit(1)
    
    analyze_section(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]))
```

### Emacs Paren Checking

If you have Emacs:

```bash
emacs --batch src/cns.lisp --eval '(check-parens)' 2>&1
```

## Prevention Strategies

### 1. Make Small Changes

Don't replace 50+ lines at once. Break it up:
1. Replace line A
2. Test load
3. Replace line B  
4. Test load

### 2. Use Version Control

```bash
# Before making changes
cp src/cns.lisp src/cns.lisp.backup-before-my-fix

# After each successful change
git add src/cns.lisp
git commit -m "Step 1: Add new case"
```

### 3. Keep Structural Balance

When adding a new `cond` case:

```lisp
# Original
(cond
  (case-1 ...)
  (t ...))

# Adding case-2 - preserve structure
(cond
  (case-1 ...)
  (case-2 ...)  ; <-- same pattern as case-1
  (t ...))      ; <-- same closing as before
```

### 4. Use Editor Paren Matching

In Vim: `%` to jump between matching parens  
In Emacs: `C-M-f` / `C-M-b` to navigate by s-expressions  
In VS Code: Rainbow brackets extension

## Emergency Recovery

If you're completely stuck:

### Option 1: Binary Search Restore

```bash
# Find exactly which line broke it
git log --oneline src/cns.lisp  # Find last working commit
git diff COMMIT src/cns.lisp > my-changes.patch

# Apply changes incrementally
git checkout COMMIT -- src/cns.lisp  # Start from working version
# Apply patch section by section
```

### Option 2: Compare-and-Fix

```bash
# Extract both structures
sed -n 'START,ENDp' working.lisp > working-section.lisp
sed -n 'START,ENDp' broken.lisp > broken-section.lisp

# Visual diff
diff -y working-section.lisp broken-section.lisp | less
```

### Option 3: Use Lisp Formatter

Some Lisp formatters can detect structural issues:

```bash
# If you have it installed
lisp-format --check src/cns.lisp
```

## Key Lessons Learned

### ✅ DO:
- Test loading after every significant change
- Trace structural nesting, not just count parens
- Compare net balance to original structure
- Use version control religiously
- Take breaks when frustrated (fresh eyes help!)

### ❌ DON'T:
- Trust raw paren counts
- Make large multi-line changes without testing
- Randomly add/remove parens hoping it works
- Assume edit tools preserve structure perfectly
- Debug when exhausted (mistakes multiply)

## When to Ask for Help

Signs you need another perspective:
- Spent >30 minutes on same paren issue
- File structure seems correct but still won't load
- Changes work in isolation but not together
- Getting frustrated and making random changes

**Take a break, document what you tried, then ask for help!**

## Future Improvements

Ideas for preventing these issues:

1. **Pre-commit Hook:** Auto-check file loads before committing
2. **Validation Script:** CNS script to validate CNS's own code
3. **Structure Visualizer:** Tool to show nesting depth
4. **Test Suite:** Auto-run after changes to catch breaks immediately

(See next section for validation script implementation)

---

## Appendix: Quick Reference

### Common Error Messages

| Error | Meaning | Action |
|-------|---------|--------|
| `end of file` | Unclosed form | Missing closing paren(s) |
| `unexpected )` | Extra closing paren | Too many closing parens |
| `undefined function` | File didn't load | Fix syntax error first |
| `READ error` | Parser couldn't read | Syntax error somewhere |

### Line Counting Commands

```bash
# Get total lines
wc -l file.lisp

# Extract specific lines  
sed -n '100,200p' file.lisp

# Show with line numbers
cat -n file.lisp | sed -n '100,200p'

# Count parens in range
sed -n '100,200p' file.lisp | tr -cd '(' | wc -c  # Opens
sed -n '100,200p' file.lisp | tr -cd ')' | wc -c  # Closes
```

### Testing Commands

```bash
# Quick load test
sbcl --non-interactive --load file.lisp 2>&1 | tail -20

# Get line/position of error
sbcl --non-interactive --load file.lisp 2>&1 | grep "line:"

# Check if function gets defined
sbcl --non-interactive --eval "(load \"file.lisp\")" \
     --eval "(fboundp 'my-function)" --eval "(quit)"
```

---

*Based on real debugging session from If/Otherwise fix (2025-11-02)*  
*Time spent on paren issues: ~2 hours*  
*Final solution: Adding 1 closing paren to line 3718*
