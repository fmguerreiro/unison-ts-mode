# Unison Tree-sitter mode for Emacs

This project aims to provide a major mode for the Unison programming language in Emacs, leveraging the power of Tree Sitter to provide syntax highlighting, code folding, indentation and other advanced editor features.

### Sample highlighting

<img width="431" alt="スクリーンショット 2023-11-23 15 27 08" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/1ca84b2f-0cda-41d0-9885-6c3758fdd46c">

<img width="470" alt="スクリーンショット 2023-11-23 14 25 26" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/ee2a6bfd-eafc-4e7f-9ee8-c29797f3876b">

## Installation

1. Add `unison-ts-mode` using your preferred method.

- Example using straight

``` elisp
(straight-use-package
 '(unison-ts-mode :type git :host github :repo "fmguerreiro/unison-ts-mode" :files ("*.el")))
```

2. Ensure you have tree-sitter installed. You can find installation instructions [here](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

3. Upon loading a .u file, Emacs will complain about not finding the grammar. 
We're gonna install [kylegoetz's grammar](https://github.com/kylegoetz/tree-sitter-unison) here. 

- Option 1, through `M-x treesit-install-language-grammar`:

First eval (or temporarily add to your config) the following expression:

``` elisp
(setq treesit-language-source-alist '((unison "https://github.com/fmguerreiro/tree-sitter-unison-kylegoetz" "build/include-parser-in-src-control")))
```

Then run `M-x treesit-install-language-grammar` and choose `unison`.

- Option 2, build it manually:

``` sh
git clone https://github.com/kylegoetz/tree-sitter-unison.git
cd tree-sitter-unison
npm install; npm run start

# figure out what kind of extension we need
if [ "$(uname)" = "Darwin" ]; then soext="dylib"; elif uname | grep -q "MINGW" > /dev/null; then soext="dll"; else soext="so"; fi

cd src
cc -fPIC -c -I. parser.c
cc -fPIC -c -I. scanner.c
cc -fPIC -shared *.o -o "unison.${soext}"
```

Now that the binary is built, we can just move it over to where the tree-sitter implementation is looking for them.
In my case, I am using [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs) package and [Doom](https://github.com/doomemacs/doomemacs), so I can move the binary over with:

``` sh
cp "unison.${soext}" ~/doom-emacs/.local/straight/build-30.0.50/tree-sitter-langs/bin
```

Your case might be different depending on your particular Emacs configuration.

NOTE: If you get an error about incompatible ABI versions, open up the `package.json` file and change the "start" script from `"start": "tree-sitter generate,` to `"start": "tree-sitter generate --abi=13",` and redo the build/copy steps.

## Usage

Once the Unison mode is installed, it will automatically activate whenever you open a file with the `.u` extension.

The Unison mode provides the following features:
- Syntax highlighting for Unison code.
- Indentation and automatic formatting.

## Contributing

Contributions to this project are welcome. Please follow the standard GitHub workflow for submitting pull requests.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Contributors

This mode depends on the Tree Sitter grammar work of [@kylegoetz](https://github.com/kylegoetz/tree-sitter-unison).
