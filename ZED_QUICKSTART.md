# Zed Editor Setup for CNS - Quick Start

## TL;DR - Fix the Errors

**Change from YAML to Python mode** to get colorization WITHOUT validation errors.

### In Zed Settings (`Ctrl+,` or `Cmd+,`)

Replace your current CNS settings with:

```json
{
  "file_types": {
    "Python": ["cns"]
  },
  "languages": {
    "Python": {
      "tab_size": 2,
      "hard_tabs": false,
      "format_on_save": "off"
    }
  }
}
```

**Then restart Zed** (`Ctrl+Q` / `Cmd+Q` and reopen)

## Why Python Mode?

✅ **No validation errors** - Python doesn't complain about CNS syntax  
✅ **Good colorization** - Keywords, strings, numbers all highlighted  
✅ **Indentation-aware** - Matches CNS's 2-space indent style  
✅ **Comment support** - `#` comments work perfectly  

## What You'll See

| CNS Code | Color/Style |
|----------|-------------|
| `Story:`, `Given:`, `Step` | Normal text (white/light) |
| `If`, `Then`, `Return` | **Purple** (Python keywords) |
| `"strings in quotes"` | **Green/Yellow** |
| Numbers: `8080`, `1`, `2` | **Orange/Blue** |
| `# comments` | **Gray/Dim** |
| `socket-listen`, `Print` | **Cyan/Blue** (function-like) |
| Variables: `port`, `server` | Normal text or light blue |
| Arrows: `→` | Normal text (unicode displays fine) |

## No More Errors!

YAML mode was throwing "Implicit keys need to be on a single line" because:
- `Step 1 → Create server socket` isn't valid YAML
- YAML expects `key: value` on one line
- YAML doesn't understand CNS narrative syntax

Python mode treats it as code, not structured data, so no validation = no errors!

## Test It

1. Open `examples/demo-webserver.cns`
2. Should see colorized code throughout
3. No red squiggly lines or error messages
4. Strings, numbers, and some keywords highlighted

## Alternatives (If You Don't Like Python Mode)

### Plain Text Mode (No Colors, But No Errors)

```json
{
  "file_types": {
    "Plain Text": ["cns"]
  }
}
```

### Markdown Mode (Some Colors, Some Errors)

```json
{
  "file_types": {
    "Markdown": ["cns"]
  }
}
```

Markdown is gentler than YAML but may still flag some syntax as odd.

## Full Details

See `EDITOR_SETUP.md` for complete documentation and future plans for a proper CNS language server.

---

**Bottom line**: Use **Python mode** for the best Zed experience with CNS right now!
