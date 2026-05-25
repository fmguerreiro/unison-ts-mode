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

Requires the Unison tree-sitter grammar from `https://github.com/kylegoetz/tree-sitter-unison`, pinned to commit `662bf52b966108cf299090a238cd6abfb65d5170` via `unison-ts-grammar-revision`. Grammar installs automatically when opening a `.u` file (controlled by `unison-ts-grammar-install` customization).

Do not bump the pin to a newer upstream commit without testing on Emacs 29 and 30. Commits after `b2ae57b` (2026-02-05) were regenerated with a newer tree-sitter CLI and misparse `let`/`handle` expressions on the tree-sitter runtime bundled with Emacs 29 and some Emacs 30 builds.

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
