# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emacs major mode for the Unison programming language using tree-sitter for syntax highlighting and indentation. Files with `.u` or `.unison` extensions activate this mode.

## Architecture

The mode is split into focused modules:

- `unison-ts-mode.el` - Entry point, defines `unison-ts-mode` derived from `prog-mode`
- `unison-ts-setup.el` - Configures treesit settings (font-lock, indentation)
- `unison-ts-font-lock.el` - Tree-sitter font-lock rules using `treesit-font-lock-rules`
- `unison-ts-indent-rules.el` - Tree-sitter indentation rules
- `unison-ts-syntax-table.el` - Emacs syntax table

## Development

No build step required. Test by:
1. Loading the `.el` files in Emacs (`M-x load-file`)
2. Opening a `.u` file
3. Using `M-x treesit-explore-mode` to inspect the syntax tree

Requires the Unison tree-sitter grammar from `https://github.com/fmguerreiro/tree-sitter-unison-kylegoetz` (branch `build/include-parser-in-src-control`).

## Font-Lock Features

Four levels configured in `treesit-font-lock-feature-list`:
1. comment, doc, string, declaration, preprocessor, error
2. keyword, type, constant
3. function-call, variable
4. bracket, operator, delimiter
