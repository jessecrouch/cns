# Quick CNS Generation Template

You are a CNS code generator. Generate ONLY valid CNS code following the exact format below.
Do NOT use nested step numbers (like 2.1, 2.2). Each Step must be numbered sequentially (Step 1, Step 2, Step 3, etc.).

## CNS Format

```
Story: <what the code does>

Given:
  <var>: <Type> = <value>

Step 1 → <action>
  Because: <why>
  Then: <variable> becomes <expression>

Step 2 → <action>
  Because: <why>
  Effect: Print "<message with {variables}>"

End: Return <result>
```

## Valid Step Patterns

**Simple step:**
```
Step 1 → Do something
  Because: Reason
  Then: x becomes x + 1
```

**Print output:**
```
Step 2 → Show result
  Because: User needs to see output
  Effect: Print "Result: {x}"
```

**Conditional:**
```
Step 3 → Check if x > 0
  Because: Different behavior for positive/negative
  If x > 0
    Then: y becomes x * 2
  Otherwise:
    Then: y becomes 0
```

**Loop with repeat (counting/recursion):**
```
Step 1 → Process current value
  Because: Need to include this number
  Then: result becomes result * counter
  Then: counter becomes counter - 1

Step 2 → Check if more iterations needed
  Because: Continue until counter reaches zero
  If counter > 0
    Then: repeat from Step 1
  Otherwise: go to End
```

**Loop over single list:**
```
Step 3 → For each name in names
  Because: Process each name
  Effect: Print "Hello {name}"
```

**Loop over multiple lists (parallel):**
```
Step 4 → For each name, score in names, scores
  Because: Process paired data
  Effect: Print "{name}: {score}"
  Then: total becomes total + score
```

## Critical Rules
- Every step MUST have `Because:`
- Steps numbered sequentially: Step 1, Step 2, Step 3 (NEVER 1.1, 2.1, etc.)
- Use `Then:` for variable assignments (syntax: `Then: var becomes expression`)
- Use `Effect:` for I/O operations (Print, Write file, Create socket, Send)
- Variables in strings use curly braces: `{varname}`
- Conditionals: `If condition` (NO colon after If)
- For counting loops, use `repeat from Step N` with a counter variable
- For list iteration, use `For each item in list` (part of Step declaration, not nested)
- Use `go to End` to exit early, `repeat from Step N` to loop back
- Assignment operator is `becomes`, NOT `=`

## Example: Factorial (Iterative Pattern)

Task: "Calculate factorial of 5"

```cns
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1
  counter: Integer = 5

Step 1 → Multiply result by current counter
  Because: Each number contributes to factorial
  Then: result becomes result * counter
  Then: counter becomes counter - 1

Step 2 → Check if more numbers to multiply
  Because: Continue until we reach 1
  If counter > 0
    Then: repeat from Step 1
  Otherwise: go to End

End: Return result
```

## Example: Word Count from File

Task: "Count words in a file"

```cns
Story: Count words in a text file

Given:
  filename: String = "input.txt"
  content: String
  words: List
  count: Integer = 0

Step 1 → Read file content
  Because: Need text to analyze
  Effect: Read from file filename into content

Step 2 → Split content into words
  Because: Need individual words to count
  Then: words becomes SPLIT content BY " "

Step 3 → Count words in list
  Because: Determine total word count
  Then: count becomes length of words

Step 4 → Display result
  Because: User needs to see the count
  Effect: Print "Word count: {count}"

End: Return count
```

## Task: {TASK}

Generate valid CNS code following the format above:
