# Unison Tree-sitter mode for Emacs

This project aims to provide a major mode for the Unison programming language in Emacs, leveraging the power of Tree Sitter to provide syntax highlighting, code folding, indentation and other advanced editor features.

### Sample highlighting

<img width="431" alt="スクリーンショット 2023-11-23 15 27 08" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/1ca84b2f-0cda-41d0-9885-6c3758fdd46c">

<img width="470" alt="スクリーンショット 2023-11-23 14 25 26" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/ee2a6bfd-eafc-4e7f-9ee8-c29797f3876b">

## Installation

1. Add `unison-ts-mode` to your config files:

- Option 1: Using quelpa

``` elisp
(quelpa '(unison-ts-mode :repo "fmguerreiro/unison-ts-mode" :fetcher github))
```

- Option 2: Using Doom (in packages.el)

``` elisp
(package! unison-ts-mode :recipe (:host github :repo "fmguerreiro/unison-ts-mode" :branch "main"))
```

- Option 3: Using straight

``` elisp
(straight-use-package
 '(unison-ts-mode :type git :host github :repo "fmguerreiro/unison-ts-mode" :files ("*.el")))
```

- Option 4: Add the project to your Emacs load-path

``` sh
git clone https://github.com/fmguerreiro/unison-ts-mode
```

``` elisp
(add-to-list 'load-path "/path/to/unison-ts-mode")
```

2. Ensure you have tree-sitter installed. You can find installation instructions [here](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

3. Upon loading a .u file, Emacs will complain about not finding the grammar. 
We're gonna install [kylegoetz's grammar](https://github.com/kylegoetz/tree-sitter-unison) here. 
As of the time of writing, it does not work through `M-x treesit-install-language-grammar` by default, so we there are two options.

- Evaluate:

``` elisp
(setq treesit-language-source-alist '((unison "https://github.com/fmguerreiro/tree-sitter-unison-kylegoetz" "build/include-parser-in-src-control")))
```

<!--- This is a clone of the original project, since it doesn't include parser.c in src control, which is expected by the treesit-install-language-grammar command. This may be fixed in a future version of the grammar implementation. --->

Then run `M-x treesit-install-language-grammar` and choose `unison`.

- Or, build it manually:
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

This mode depends on the work of [@kylegoetz](https://github.com/kylegoetz/tree-sitter-unison).
