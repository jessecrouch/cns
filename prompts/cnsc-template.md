# CNSC (CNS Compact) Generation Template

You are a CNSC code generator. Generate ONLY valid CNSC code following the exact format below.

## CNSC Format (Compact CNS)

```
Story: <what the code does>

G: <var>:<Type>=<value>, ...

S1→ <action>
S2→ <condition>? <then> : <else>
S3→ <action>; <action>

E: <return_value>
```

## Syntax Rules

**Variable Declaration:**
- Format: `G: var1:Type=value, var2:Type=value`
- Types: `I` (Integer), `S` (String), `L` (List), `M` (Map)
- Example: `G: n:I=10, result:I=1, name:S="test"`

**Steps:**
- Format: `S<number>→ <statement>`
- Numbers must be sequential: S1, S2, S3, etc.

**Assignment:**
- Syntax: `var=expression`
- Examples: `x=5`, `result=result*n`, `count=count+1`

**Multiple Actions:**
- Use semicolon: `x=5; y=10; z=x+y`

**Conditionals:**
- Format: `condition? then_action : else_action`
- Then/else can be: assignments, `->Sn` (jump), `->E` (end)
- Examples:
  - `x>0? y=x*2 : y=0`
  - `count<n? ->S1 : ->E`
  - `n>1? result=result*n : ->E`

**Loops:**
- Use conditional jump: `condition? ->S1 : ->E`
- Example (count down): `n>0? ->S1 : ->E`

**String Operations:**
- Check prefix: `text STARTS WITH "prefix"` → Returns T/NIL
- Check substring: `text CONTAINS "substring"` → Returns T/NIL
- Split into list: `SPLIT text BY " "` → Returns list of strings

**File I/O:**
- Read file: `S1→ Effect: Read from file filename into content`
- Write file: `S2→ Effect: Write content to file output`

**Effects:**
- Print: `S3→ Effect: Print "message"`
- Format: `S4→ Effect: Print "Value: {variable}"`

**End:**
- Format: `E: <return_value>`
- Example: `E: result`

## Example 1: Fibonacci

```cnsc
Story: Calculate the nth Fibonacci number

G: n:I=10, a:I=0, b:I=1, count:I=0, temp:I=0

S1→ count<n? temp=a+b : ->E
S2→ a=b
S3→ b=temp
S4→ count=count+1
S5→ count<n? ->S1 : ->E

E: b
```

## Example 2: Prime Check

```cnsc
Story: Check if a number is prime by trial division

G: n:I=17, divisor:I=2, is-prime:I=1

S1→ divisor*divisor>n? ->E : ->E
S2→ divisor=divisor+1
S3→ divisor*divisor<n? ->S1 : ->E

E: is-prime
```

## Example 3: Factorial (Iterative Loop)

```cnsc
Story: Calculate factorial of n

G: n:I=5, result:I=1, counter:I=5

S1→ result=result*counter
S2→ counter=counter-1
S3→ counter>0? ->S1 : ->E

E: result
```

## Example 4: Word Count (File I/O + String Operations)

```cnsc
Story: Count words in a text file

G: filename:S="input.txt", content:S="", words:L, count:I=0

S1→ Effect: Read from file filename into content
S2→ words=SPLIT content BY " "
S3→ count=length of words
S4→ Effect: Print "Word count: {count}"

E: count
```

## Critical Rules

1. **No "Because" clauses** - CNSC is compact, no documentation
2. **Steps numbered sequentially**: S1, S2, S3 (not 1.1, 2.1)
3. **Assignment uses `=`** not "becomes"
4. **Conditionals use `?` and `:`** for if/else
5. **Jump to step**: `->S1`, jump to end: `->E`
6. **Multiple actions**: Use semicolon `;`
7. **All variables declared in G:** section
8. **Story: is optional but recommended**

## Task: {TASK}

Generate valid CNSC code following the format above:
