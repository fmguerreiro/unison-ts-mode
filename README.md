# unison-ts-mode

Emacs major mode for [Unison](https://www.unison-lang.org/) using tree-sitter.

**Status**: Ready for MELPA submission. Once accepted, install with `M-x package-install RET unison-ts-mode`.

## Quick Start

1. Install the package (see [Installation](#installation))
2. Open a `.u` file - you'll be prompted to install the tree-sitter grammar
3. (Optional) Enable LSP: `M-x eglot` or `M-x lsp`

## Features

- Syntax highlighting with tree-sitter (4 customizable levels)
- Automatic indentation for all Unison constructs
- imenu support for navigation (functions, types, abilities)
- LSP integration (eglot and lsp-mode)
- Auto-install of tree-sitter grammar

## Screenshots

<img width="431" alt="Syntax highlighting example 1" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/1ca84b2f-0cda-41d0-9885-6c3758fdd46c">

<img width="470" alt="Syntax highlighting example 2" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/ee2a6bfd-eafc-4e7f-9ee8-c29797f3876b">

## Requirements

- Emacs 29+ (with native tree-sitter support)
- [Unison tree-sitter grammar](https://github.com/fmguerreiro/tree-sitter-unison) (auto-installed)

## Installation

### Package

**use-package + straight:**

```elisp
(use-package unison-ts-mode
  :straight (:host github :repo "fmguerreiro/unison-ts-mode")
  :mode ("\\.u\\'" "\\.unison\\'"))
```

**straight.el:**

```elisp
(straight-use-package
  '(unison-ts-mode :type git :host github :repo "fmguerreiro/unison-ts-mode"))
```

**Doom Emacs:**

```elisp
;; packages.el
(package! unison-ts-mode :recipe (:host github :repo "fmguerreiro/unison-ts-mode"))
```

### Grammar

The tree-sitter grammar installs automatically when you first open a `.u` file. Customize behavior with `unison-ts-grammar-install`:
- `'prompt` (default): Ask before installing
- `'auto`: Install automatically
- `nil`: Never auto-install

Manual install: `M-x unison-ts-install-grammar`

**Custom grammar source:**

```elisp
(setq unison-ts-grammar-repository "https://github.com/yourname/tree-sitter-unison")
(setq unison-ts-grammar-revision "your-branch")  ; Optional
```

## Configuration

### Font-lock Levels

Customize highlighting depth via `treesit-font-lock-level`:

1. comment, doc, string, declaration, preprocessor, error
2. keyword, type, constant
3. function-call, variable
4. bracket, operator, delimiter

Apply changes: `M-x treesit-font-lock-recompute-features`

### imenu Navigation

Navigate to functions, types, and abilities:

- `M-x imenu` - Jump to definition
- `M-x which-function-mode` - Show current function in mode line
- Works with helm-imenu, counsel-imenu, consult-imenu

```elisp
;; Auto-enable which-function-mode
(add-hook 'unison-ts-mode-hook 'which-function-mode)
```

### LSP Support

Requires [UCM](https://www.unison-lang.org/docs/install-instructions/). UCM auto-starts in headless mode when you open a `.u` file.

**Eglot (built-in Emacs 29+):**

```elisp
(add-hook 'unison-ts-mode-hook 'eglot-ensure)
```

**lsp-mode:**

```elisp
(add-hook 'unison-ts-mode-hook 'lsp-deferred)
```

**Custom port:**

```bash
export UNISON_LSP_PORT=5758
```

**Windows:** LSP is disabled by default. Enable it:

```powershell
[System.Environment]::SetEnvironmentVariable('UNISON_LSP_ENABLED','true')
```

**Manual UCM:**

```bash
ucm headless
```

## Troubleshooting

**Grammar installation fails:**

Install build tools:
```sh
# macOS
xcode-select --install

# Debian/Ubuntu
sudo apt-get install build-essential git

# Fedora/RHEL
sudo dnf install gcc git
```

**ABI version mismatch:**

```sh
tree-sitter generate --abi=13
```

**Grammar not found:**

Check `~/.emacs.d/tree-sitter/` or `treesit-extra-load-path` contains the compiled grammar.

**LSP connection refused:**

- Verify `ucm` is in PATH: `which ucm`
- Check port 5757: `lsof -i :5757` (macOS/Linux) or `netstat -an | findstr 5757` (Windows)
- Start manually: `ucm headless`
- Check logs: `*EGLOT events*` (eglot) or `*lsp-log*` (lsp-mode)

**LSP features not working:**

Ensure you're in a valid Unison codebase directory.

## Advanced

### Manual Grammar Build

If auto-install fails:

```sh
git clone https://github.com/fmguerreiro/tree-sitter-unison.git
cd tree-sitter-unison

# Determine shared library extension
if [ "$(uname)" = "Darwin" ]; then soext="dylib"
elif uname | grep -q "MINGW"; then soext="dll"
else soext="so"; fi

cd src
cc -fPIC -c -I. parser.c
cc -fPIC -c -I. scanner.c
cc -fPIC -shared *.o -o "libtree-sitter-unison.${soext}"

# Copy to Emacs tree-sitter directory
mkdir -p ~/.emacs.d/tree-sitter
cp "libtree-sitter-unison.${soext}" ~/.emacs.d/tree-sitter/
```

### Syntax Tree Inspection

Use `M-x treesit-explore-mode` to inspect the syntax tree while developing or debugging.

## Contributing

Contributions welcome via GitHub pull requests.

## License

GPL-3.0 License. See [LICENSE](LICENSE) for details.

## Credits

Tree-sitter grammar by [@kylegoetz](https://github.com/kylegoetz/tree-sitter-unison).
