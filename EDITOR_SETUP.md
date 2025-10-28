# CNS Editor Configuration for Zed

This guide shows you how to set up syntax highlighting for `.cns` files in Zed editor.

## Quick Setup

Since Zed doesn't have native CNS support yet, we'll use a workaround by treating CNS files as a custom language or associating them with an existing language that highlights similar patterns.

### Option 1: Use Markdown Mode (Simplest)

Add to your Zed settings (`~/.config/zed/settings.json`):

```json
{
  "file_types": {
    "Markdown": ["cns"]
  }
}
```

This gives you basic highlighting for:
- Section headers (Story:, Given:, Step, End:)
- Code-like patterns
- Strings and numbers

### Option 2: Use Python Mode (Best Balance - RECOMMENDED)

```json
{
  "file_types": {
    "Python": ["cns"]
  },
  "languages": {
    "Python": {
      "format_on_save": "off"
    }
  }
}
```

This highlights:
- Keywords (If, Then, Return, etc.)
- Indentation structure (Python is indent-aware like CNS)
- Strings and numbers
- Function calls
- **No syntax errors** (unlike YAML)

### Option 3: Custom Language Configuration (Best, but requires setup)

1. **Add to your Zed settings** (`~/.config/zed/settings.json`):

```json
{
  "languages": {
    "Plain Text": {
      "tab_size": 2,
      "hard_tabs": false
    }
  },
  "file_types": {
    "Plain Text": ["cns"]
  },
  "file_scan_exclusions": []
}
```

2. **Create a custom colorization file** in your CNS project:

Create `.zed/settings.json` in your CNS project directory with this content:

```json
{
  "languages": {
    "CNS": {
      "tab_size": 2,
      "hard_tabs": false,
      "soft_wrap": "none",
      "format_on_save": "off"
    }
  },
  "file_types": {
    "CNS": ["cns"]
  }
}
```

## Recommended: Python Mode Setup (No Errors!)

For the best experience without validation errors, I recommend **Python mode**:

1. Open Zed settings: `Ctrl+,` (or `Cmd+,` on Mac)
2. Add this to your settings:

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

3. Save and restart Zed

**Why Python mode?** Python's syntax highlighter works great for CNS:
- Keywords like `Given:`, `If`, `Then:` are highlighted
- Strings in quotes are colored
- Numbers are highlighted
- Comments with `#` work perfectly
- No validation errors (unlike YAML)
- Indentation-aware (like CNS)

## What Gets Highlighted (Python Mode)

- **Keywords**: `Given:`, `If`, `Then:`, `Return` (highlighted as Python keywords)
- **Strings**: `"quoted text"` in green/yellow
- **Numbers**: `8080`, `1`, `2` in orange/blue
- **Variables**: `port`, `server`, etc. in white/blue
- **Arrows**: `→` (displayed but not specially colored)
- **Comments**: Lines starting with `#` in gray
- **Function-like**: `socket-listen`, `Print` highlighted as functions
- **No validation errors**: Unlike YAML, Python mode won't complain about CNS syntax

## Example Appearance

```cns
Story: Start a simple HTTP server          # Comment in gray

Given:                                     # Keyword in purple
  port: Integer = 8080                     # Variable in blue, number in orange
  server: Socket                           # Type in cyan
  message: String = "Hello World"          # String in green

Step 1 → Listen for connections            # Keyword + arrow + description
  Because: We need to accept HTTP requests # Keyword in purple
  Effect: socket-listen(server, port)      # Function in yellow/cyan

End: Return 0                              # Keyword in purple
```

## Testing Your Setup

1. Open any `.cns` file in the `examples/` directory
2. Check if keywords like `Story:`, `Given:`, `Because:` are highlighted
3. Verify strings in quotes are colored differently
4. Numbers should appear in a distinct color

## Full Language Server (Future)

For complete syntax support (autocomplete, error checking, formatting), we'll need:

1. **Tree-sitter grammar** for CNS (complex, ongoing work)
2. **Language Server Protocol (LSP)** implementation
3. Zed extension package

The current YAML mode setup is the best practical solution until then.

## Troubleshooting

**Q: Changes don't appear after editing settings?**  
A: Restart Zed completely (`Cmd/Ctrl+Q` and reopen)

**Q: YAML mode shows "Implicit keys need to be on a single line" errors?**  
A: Switch to Python mode instead - YAML's validator is too strict for CNS syntax. Python mode gives better highlighting without errors.

**Q: Want different colors?**  
A: Change your Zed theme: Settings → Theme → Select a different theme (One Dark, Dracula, etc.)

## Alternative Editors

- **VS Code**: Create a TextMate grammar in `.vscode/extensions/`
- **Vim/Neovim**: Create a `syntax/cns.vim` file
- **Emacs**: Create a major mode `cns-mode.el`

Let me know if you want configurations for these editors too!
