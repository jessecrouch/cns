# CNS Quick Start Guide

Welcome to Causal Narrative Script! Here's how to get started in 5 minutes.

## 1. Check Installation

Make sure you have SBCL (Steel Bank Common Lisp):
```bash
sbcl --version
```

## 2. Run Your First Program

```bash
./cns-run examples/factorial.cns
```

You should see:
```
=== Executing Story: Compute factorial of a positive integer ===
...
Return: 120
```

## 3. Try More Examples

See what's available:
```bash
./cns-run --list
```

Run them all:
```bash
./cns-run --all
```

## 4. Write Your Own Program

Create a file `my-program.cns`:

```cns
Story: Double a number

Given:
  x: Integer = 21

Step 1 → x becomes x + x
  Because: we want to double the value

End: Return x
```

Run it:
```bash
./cns-run my-program.cns
```

## 5. Use the Interactive REPL

Start the REPL:
```bash
./cns-run --repl
```

Type your program, end with `.` on a line by itself, then see it run!

## Common Commands

| Command | What It Does |
|---------|-------------|
| `./cns-run file.cns` | Run a CNS file |
| `./cns-run --list` | List all examples |
| `./cns-run --all` | Run all examples |
| `./cns-run --repl` | Interactive mode |
| `./cns-run --test` | Run test suite |
| `./cns-run --help` | Show help |

## CNS Syntax Cheat Sheet

```cns
Story: Brief description of what this program does

Given:
  variable_name: Type = initial_value

Step 1 → Action description
  Because: Why this step is necessary
  Then: variable becomes new_value

Step 2 → If condition
  Because: Why we check this condition
  Then: repeat from Step 1
  Otherwise: go to End

End: Return variable_name
```

## Key Features

- **Because:** - Every step explains WHY (mandatory!)
- **Then:** - Shows what changes
- **If/Otherwise:** - Branching logic
- **repeat from Step N** - Loops
- **Arrow (→)** - Separates step number from action

## Tips

1. **Start Simple** - Begin with the examples
2. **Be Explicit** - CNS values clarity over brevity
3. **Explain Causality** - The "Because:" is what makes CNS special
4. **Watch It Run** - The verbose output helps you understand execution
5. **Think Narratively** - Write code like you're telling a story

## Next Steps

1. Read `examples/README.md` for detailed explanations
2. Modify existing examples to see how they work
3. Check out `AGENTS.md` for the full language design
4. Write your own algorithms!

## Getting Help

- Run `./cns-run --help` for command options
- Check the examples directory for patterns
- Read the main README.md for complete documentation

Happy coding! Remember: In CNS, every line tells a story. 📖✨
