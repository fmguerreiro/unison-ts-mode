# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emacs major mode for the Unison programming language using tree-sitter for syntax highlighting and indentation. Requires Emacs 29+ with native tree-sitter support. Files with `.u` or `.unison` extensions activate this mode.

## Architecture

The mode is split into focused modules:

- `unison-ts-mode.el` - Entry point, defines `unison-ts-mode` derived from `prog-mode`, syntax table, treesit setup, LSP integration (eglot/lsp-mode), imenu settings, keybindings
- `unison-ts-font-lock.el` - Tree-sitter font-lock rules using `treesit-font-lock-rules`
- `unison-ts-indent-rules.el` - Tree-sitter indentation rules
- `unison-ts-install.el` - Grammar auto-installation
- `unison-ts-repl.el` - UCM (Unison Codebase Manager) integration via MCP (Model Context Protocol)

## Development Commands

```bash
# Run tests (requires Unison grammar installed)
emacs --batch -L . -l ert -l unison-ts-mode-tests.el -f ert-run-tests-batch-and-exit

# Run a single test
emacs --batch -L . -l ert -l unison-ts-mode-tests.el --eval '(ert-run-tests-batch-and-exit "unison-ts-font-lock/keyword-if")'

# Byte-compile all files
emacs --batch -L . -f batch-byte-compile unison-ts-mode.el unison-ts-font-lock.el unison-ts-indent-rules.el unison-ts-install.el unison-ts-repl.el

# Run pre-commit checks (MELPA-style)
./scripts/pre-commit
```

## Manual Testing

1. Load the `.el` files in Emacs (`M-x load-file`)
2. Open a `.u` file (example: `examples/test.u`)
3. Use `M-x treesit-explore-mode` to inspect the syntax tree

## Tree-sitter Grammar

Requires the Unison tree-sitter grammar from `https://github.com/kylegoetz/tree-sitter-unison`, pinned to commit `10365cc70ab2b2de85ea7ab35cf6b7636c36ce8b` via `unison-ts-grammar-revision`. Grammar installs automatically when opening a `.u` file (controlled by `unison-ts-grammar-install` customization).

## UCM Keybindings

All under `C-c C-u` prefix:
- `C-c C-u r` - Open UCM REPL
- `C-c C-u a` - Add definitions
- `C-c C-u u` - Update definitions
- `C-c C-u t` - Run tests
- `C-c C-u v` - Evaluate expression (works for pure functions)
- `C-c C-u x` - Run IO action
- `C-c C-u w` - Watch/typecheck current file
- `C-c C-u l` - Load current file into codebase
