# Installing Regex Support for CNS

CNS v1.2.0 includes regex support through the `MATCHES` and `EXTRACT` operators. These features require the CL-PPCRE (Perl-Compatible Regular Expressions) library.

## Quick Install

### Option 1: Using Quicklisp (Recommended)

If you have Quicklisp installed:

```bash
sbcl --eval "(ql:quickload :cl-ppcre)" --eval "(quit)"
```

### Option 2: Using ASDF

If you have ASDF configured:

```bash
sbcl --eval "(asdf:load-system :cl-ppcre)" --eval "(quit)"
```

### Option 3: Manual Installation

1. Download cl-ppcre from: https://github.com/edicl/cl-ppcre
2. Extract to your Common Lisp library directory
3. Load with: `(load "path/to/cl-ppcre/load.lisp")`

## Installing Quicklisp

If you don't have Quicklisp installed:

```bash
# Download Quicklisp installer
curl -O https://beta.quicklisp.org/quicklisp.lisp

# Install Quicklisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --eval "(quit)"

# Now install cl-ppcre
sbcl --eval "(ql:quickload :cl-ppcre)" --eval "(quit)"
```

## Verifying Installation

Run a test CNS script:

```bash
./cns-run examples/test-regex.cns
```

If cl-ppcre is installed, you should see regex operations working.
If not installed, you'll see:

```
Regex support unavailable (cl-ppcre not found)
Install with: (ql:quickload :cl-ppcre)
MATCHES and EXTRACT operators will be unavailable.
```

## Graceful Degradation

CNS gracefully handles missing cl-ppcre:
- Core CNS features work without it
- HTTPS, JSON, string operations, etc. all function normally
- Only MATCHES and EXTRACT operators require cl-ppcre
- Clear error messages guide users to install it

## Regex Operators

Once installed, you can use:

### MATCHES Operator

Pattern matching that returns boolean:

```cns
Effect: is_valid = email MATCHES "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
Effect: has_phone = text MATCHES "\\d{3}-\\d{3}-\\d{4}"
```

### EXTRACT Operator

Extract first match or specific capture groups:

```cns
# Extract first match
Effect: phone = EXTRACT "\\d{3}-\\d{3}-\\d{4}" FROM text

# Extract specific capture group
Effect: date = EXTRACT "\\[(\\d{4}-\\d{2}-\\d{2})" GROUP 1 FROM log_line
Effect: time = EXTRACT "\\[(\\d{4}-\\d{2}-\\d{2}) (\\d{2}:\\d{2}:\\d{2})\\]" GROUP 2 FROM log_line

# GROUP 0 (or omit GROUP) returns full match
Effect: full_match = EXTRACT "\\d{3}-\\d{3}-\\d{4}" GROUP 0 FROM text
```

### Pattern Syntax

CNS uses Perl-compatible regex (PCRE) syntax:
- `\\d` - digit (0-9)
- `\\w` - word character (a-z, A-Z, 0-9, _)
- `\\s` - whitespace
- `^` - start of string
- `$` - end of string
- `+` - one or more
- `*` - zero or more
- `?` - zero or one
- `()` - capture group
- `[]` - character class
- `|` - alternation (OR)

Note: Double backslash `\\` is required in CNS strings to escape regex special characters.

## Examples

See comprehensive examples:
- `examples/test-regex.cns` - Full test suite with 15 test cases
- `examples/test-regex-simple.cns` - Quick demonstration

## Troubleshooting

### "Don't know how to REQUIRE CL-PPCRE"

This means cl-ppcre is not in your Common Lisp environment. Install via Quicklisp (see above).

### "Package CL-PPCRE does not exist"

Quicklisp may not be configured. Install Quicklisp first (see Installing Quicklisp section).

### Still Having Issues?

1. Check your SBCL version: `sbcl --version` (should be 2.0.0+)
2. Check Quicklisp is loaded: `sbcl --eval "(ql:system-apropos \"ppcre\")"`
3. Open an issue: https://github.com/jessecrouch/cns/issues

## Performance Notes

CL-PPCRE is a high-performance regex engine:
- Compiled regex patterns for speed
- Handles complex patterns efficiently
- Suitable for production use

Regex operations in CNS have minimal overhead beyond the CL-PPCRE library itself.
