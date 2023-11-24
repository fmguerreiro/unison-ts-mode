# Unison Tree-sitter mode for Emacs

This project aims to provide a major mode for the Unison programming language in Emacs, leveraging the power of Tree Sitter to provide syntax highlighting, code folding, indentation and other advanced editor features.

Depends on the work of [kylegoetz's tree-sitter unison grammar implementation](https://github.com/kylegoetz/tree-sitter-unison), so ensure that is installed (instructions below as well).

### Sample highlighting

<img width="431" alt="スクリーンショット 2023-11-23 15 27 08" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/1ca84b2f-0cda-41d0-9885-6c3758fdd46c">

<img width="470" alt="スクリーンショット 2023-11-23 14 25 26" src="https://github.com/fmguerreiro/unison-ts-mode/assets/14042481/ee2a6bfd-eafc-4e7f-9ee8-c29797f3876b">

## Installation

1. Clone or download this project to your local machine.

2. Ensure that you have Tree Sitter installed. You can find installation instructions [here](https://emacs-tree-sitter.github.io/).

3. Add the project directory to your Emacs load-path.

    ```elisp
    (add-to-list 'load-path "/path/to/unison-ts-mode")
    ```

4. Require the `unison-ts-mode` package in your Emacs init file.

    ```elisp
    (require 'unison-ts-mode)
    ```

5. Associate the `.u` file extension with the Unison mode.

    ```elisp
    (add-to-list 'auto-mode-alist '("\\.u\\'" . unison-ts-mode))
    ```

6. Restart Emacs or evaluate the added code to load the Unison mode.

## Usage

Once the Unison mode is installed, it will automatically activate whenever you open a file with the `.u` extension.

The Unison mode provides the following features:
- Syntax highlighting for Unison code.
- Code folding to hide or reveal blocks of code.
- Navigation through code using Tree Sitter's syntax tree.
- Indentation and automatic formatting.

In addition to the default keybindings provided by Emacs, the Unison mode includes the following keybindings:

- `C-c C-r` (`unison-reload`) - Reload the syntax tree for the current buffer.
- `C-c C-f` (`unison-fold-toggle`) - Toggle code folding for the current code block.

Refer to the Unison mode documentation for more information on available commands, keybindings, and customization options.

## Contributing

Contributions to this project are welcome. Please follow the standard GitHub workflow for submitting pull requests.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
