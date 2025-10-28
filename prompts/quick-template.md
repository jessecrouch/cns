# Quick CNS Generation Template

## Format

```cns
Story: <what the code does>

Given:
  <var>: <Type> [<tag>] = <value>

Step 1 → <action>
  Because: <why>
  Effect: <side-effect> (if needed)
  Then: <state-change> (if needed)

End: Return <result>
```

## Rules
- Every step needs `Because:`
- Use `Effect:` for I/O (Print, Write, Create socket, Send, etc.)
- Use `Then:` for variable changes
- Use `If/Then/Otherwise` for conditions
- Variables in effects: `{varname}`

## Task: {TASK}

Generate CNS code:
