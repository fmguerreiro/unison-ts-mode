# unison-ts-mode

Emacs major mode for [Unison](https://www.unison-lang.org/) using tree-sitter.

## Screenshots

<img width="431" alt="Syntax highlighting example 1" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/1ca84b2f-0cda-41d0-9885-6c3758fdd46c">

<img width="470" alt="Syntax highlighting example 2" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/ee2a6bfd-eafc-4e7f-9ee8-c29797f3876b">

## Requirements

- Emacs 29+ (with native tree-sitter support)
- [Unison tree-sitter grammar](https://github.com/fmguerreiro/tree-sitter-unison)

## Installation

### Grammar

```elisp
(add-to-list 'treesit-language-source-alist
  '(unison "https://github.com/fmguerreiro/tree-sitter-unison"))
```

Then `M-x treesit-install-language-grammar RET unison`.

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

### Manual Grammar Build

If you prefer to build the grammar manually:

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

## Features

- Syntax highlighting (4 customizable levels via `treesit-font-lock-level`)
- Indentation
- `treesit-explore-mode` support for syntax tree inspection

## Font-lock Levels

Customize highlighting depth with `M-x treesit-font-lock-recompute-features` or set `treesit-font-lock-level`:

1. comment, doc, string, declaration, preprocessor, error
2. keyword, type, constant
3. function-call, variable
4. bracket, operator, delimiter

## Troubleshooting

**ABI version mismatch:** If you encounter ABI compatibility errors, rebuild the grammar with the `--abi=13` flag:

```sh
tree-sitter generate --abi=13
```

**Grammar not found:** Ensure the compiled grammar is in a directory listed in `treesit-extra-load-path` or the default `~/.emacs.d/tree-sitter/`.

## Contributing

Contributions welcome. Please follow standard GitHub workflow for pull requests.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Credits

Tree-sitter grammar by [@kylegoetz](https://github.com/kylegoetz/tree-sitter-unison).
