# Grammar Auto-Install Design

## Architecture

**unison-ts-install.el** - New file handling installation logic
- `unison-ts-ensure-grammar` - Check and install if needed
- `unison-ts-install-grammar` - Interactive install command
- Session caching to avoid repeated prompts

**unison-ts-mode.el** - Modified to call ensure-grammar on activation

## Configuration

```elisp
(defcustom unison-ts-grammar-install 'prompt
  "Control automatic grammar installation.
- prompt: Ask before installing
- auto: Install automatically
- nil: Never auto-install")
```

## Flow

1. User opens .u file
2. Mode checks if grammar available
3. If missing and first prompt this session:
   - If `'prompt`: Ask "Install Unison grammar for syntax highlighting?"
   - If `'auto`: Install immediately
   - If `nil`: Show simple error, don't prompt
4. On yes: Auto-populate treesit-language-source-alist, call treesit-install-language-grammar
5. On success: Activate mode immediately in current buffer
6. On failure: Show error message only
7. Cache decision for session

## Implementation

- Use built-in `treesit-install-language-grammar`
- Auto-populate `treesit-language-source-alist` with correct repo URL
- Session state tracking via defvar
- Simple error messages, no help buffers
