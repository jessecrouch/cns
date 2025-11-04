# Test 1: Word Counter CLI Tool

## Task

Build a command-line word counter tool in CNS that:

1. **Accepts a filename as first positional argument** (ARGS[0])
2. **Accepts optional flags:**
   - `--lines` - Show line count instead of word count
   - `--chars` - Show character count instead of word count
   - `--verbose` - Show filename in output
3. **Default behavior** (no flags): Show word count
4. **Handles missing file** - Print error message if file doesn't exist

## Example Usage

```bash
# Count words in file (default)
./cns-run word-counter.cns input.txt
# Output: 42

# Count lines
./cns-run word-counter.cns input.txt --lines
# Output: 10

# Count characters
./cns-run word-counter.cns input.txt --chars
# Output: 256

# Verbose mode
./cns-run word-counter.cns input.txt --verbose
# Output: input.txt: 42 words

# Verbose with lines
./cns-run word-counter.cns input.txt --lines --verbose
# Output: input.txt: 10 lines

# Missing file
./cns-run word-counter.cns nonexistent.txt
# Output: Error: File not found
```

## Requirements

1. Use `ARGS[0]` to get filename
2. Use `HAS_FLAG("--lines")` to check for --lines flag
3. Use `HAS_FLAG("--chars")` to check for --chars flag
4. Use `HAS_FLAG("--verbose")` to check for --verbose flag
5. Use `FILE EXISTS` to check if file exists
6. Use `READ FROM FILE` to read file contents
7. Use `SPLIT` to split by newlines for line count
8. Use `SPLIT` to split by spaces for word count
9. Use `LENGTH OF` to get character count or list length
10. Use proper error handling

## Expected Logic Flow

1. Get filename from ARGS[0]
2. Check if file exists
   - If not: print error and exit
3. Read file contents
4. Check flags to determine what to count:
   - If --lines: split by "\n" and count
   - Else if --chars: get LENGTH OF content
   - Else: split by " " and count (default: words)
5. Format output:
   - If --verbose: include filename
   - Print count

## CNS Features to Use

- **CLI Arguments**: `ARGS[0]`, `HAS_FLAG()`
- **File Operations**: `FILE EXISTS`, `READ FROM FILE`
- **String Operations**: `SPLIT`, `LENGTH OF`
- **Conditionals**: If/Otherwise blocks for flag checking
- **String Interpolation**: For verbose output

## Success Criteria

- [ ] Code validates with `./cns-validate`
- [ ] Code handles missing file gracefully
- [ ] Default behavior counts words
- [ ] --lines flag counts lines
- [ ] --chars flag counts characters
- [ ] --verbose flag includes filename
- [ ] Can combine --verbose with other flags

---

**Generate a complete CNS program that solves this task.**

Use the CNS syntax reference below:

[INCLUDE FULL SYNTAX.MD HERE]
