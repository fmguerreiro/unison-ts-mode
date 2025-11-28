# MELPA Package Submission: unison-ts-mode

## Package Summary
Emacs major mode for the Unison programming language using tree-sitter for syntax highlighting and indentation. Provides modern IDE-like features including automatic indentation, imenu navigation, and LSP integration (eglot/lsp-mode).

## Repository
https://github.com/fmguerreiro/unison-ts-mode

## Association with Package
I am the author and maintainer of this package. This package is a fork of https://github.com/dariooddenino/unison-ts-mode-emacs with significant improvements:
- Converted to use tree-sitter instead of regex-based highlighting
- Added automatic tree-sitter grammar installation
- Improved indentation rules
- Added LSP integration support
- Comprehensive test suite

## Upstream Communications
This is a new submission. The package has been in active development and is ready for broader distribution through MELPA.

## Requirements Met
- [x] GPL-compatible license (GPL v3)
- [x] Source code in authoritative Git repository
- [x] Lexical binding enabled in all files
- [x] Function references use #' notation
- [x] Package metadata follows package.el format
- [x] All functions have proper docstrings
- [x] Passes package-lint checks
- [x] No custom faces with inherit+override antipattern
- [x] Recipe created with minimal package size

## Additional Notes
- Requires Emacs 29.1+ for tree-sitter support
- Tree-sitter grammar is auto-installed on first use
- Package is split into focused modules for maintainability
