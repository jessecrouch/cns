# CNS Examples

This directory contains example programs written in Causal Narrative Script (CNS) demonstrating various programming patterns and algorithms.

## Running Examples

Use the CNS runner from the project root:

```bash
# Run a specific example
./cns-run examples/factorial.cns

# List all examples
./cns-run --list

# Run all examples
./cns-run --all

# Get help
./cns-run --help
```

## Available Examples

### Basic Examples

**factorial.cns** - Compute factorial of a positive integer
- Input: n = 5
- Output: 120 (5! = 5×4×3×2×1)
- Demonstrates: Loops, multiplication, decrement

**simple-counter.cns** - Count from 1 to 5
- Input: counter = 1, target = 5
- Output: 5
- Demonstrates: Basic loops, conditionals, increment

**sum-numbers.cns** - Calculate sum of numbers from 1 to n
- Input: n = 10
- Output: 55 (1+2+3+...+10)
- Demonstrates: Accumulation pattern

**sum-range.cns** - Sum all integers from start to end
- Input: start = 1, end = 100
- Output: 5050
- Demonstrates: Range iteration, accumulation

### Mathematical Algorithms

**fibonacci.cns** - Calculate the nth Fibonacci number
- Input: n = 10
- Output: 55 (10th Fibonacci number)
- Demonstrates: Sequence generation, multiple state variables

**power.cns** - Calculate base raised to the power of exponent
- Input: base = 2, exponent = 8
- Output: 256 (2^8)
- Demonstrates: Repeated multiplication

**gcd.cns** - Find greatest common divisor using Euclidean algorithm
- Input: a = 48, b = 18
- Output: 6 (GCD of 48 and 18)
- Demonstrates: Classical algorithm, modulo operation concept

**is-prime.cns** - Check if a number is prime by trial division
- Input: n = 17
- Output: 1 (true, 17 is prime)
- Demonstrates: Boolean logic, optimization (only check up to √n)

### Interesting Sequences

**collatz.cns** - Count steps in the Collatz conjecture sequence
- Input: n = 27
- Output: Number of steps to reach 1
- Demonstrates: Complex conditionals, famous unsolved problem

**digit-sum.cns** - Calculate the sum of digits in a number
- Input: n = 12345
- Output: 15 (1+2+3+4+5)
- Demonstrates: Digit extraction pattern

## Understanding CNS Structure

Each example follows the CNS narrative structure:

```cns
Story: <What the program does>

Given:
  <variable>: <Type> = <initial value>

Step N → <Action or condition>
  Because: <Why this step is necessary>
  Then: <State transformation>
  Otherwise: <Alternative path>

End: Return <result>
```

## Key Features Demonstrated

- **Explicit Causality**: Every step explains "why" with `Because:` clause
- **State Transformations**: Clear `Then:` clauses showing variable changes
- **Loop Control**: `repeat from Step N` for iteration
- **Conditionals**: `If`, `Then`, `Otherwise` for branching
- **Narrative Flow**: Code reads like a story with clear intent

## Creating Your Own Examples

1. Start with a clear story describing what you want to compute
2. Declare all variables in the `Given:` section
3. Break the algorithm into logical steps
4. Explain each step's causality with `Because:`
5. Use `Then:` for state changes
6. Use conditionals for branching logic
7. End with the return value

## Tips for Writing CNS

- **Be Explicit**: Don't hide logic in complex expressions
- **Explain Causality**: The `Because:` clause is mandatory and important
- **Linear Steps**: Keep steps sequential and easy to follow
- **Single Responsibility**: Each step should do one thing well
- **Test Incrementally**: Run after each step to verify logic

## Running with Verbose Output

By default, CNS shows full execution traces. To see exactly how your program executes:

```bash
./cns-run examples/factorial.cns
```

You'll see:
- Initial variable values
- Each step's execution
- Causality explanations
- State transformations
- Loop jumps
- Final result

This makes debugging and understanding the algorithm very easy!
