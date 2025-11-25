;;; unison-ts-install.el --- Grammar installation for unison-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Handles automatic installation of the tree-sitter grammar for Unison.

;;; Code:

(defcustom unison-ts-grammar-install 'prompt
  "Control automatic grammar installation.
- prompt: Ask before installing (default)
- auto: Install automatically
- nil: Never auto-install"
  :type '(choice (const :tag "Prompt before installing" prompt)
                 (const :tag "Install automatically" auto)
                 (const :tag "Never auto-install" nil))
  :group 'unison-ts)

(defvar unison-ts--install-prompted nil
  "Whether we've already prompted for installation this session.")

(defun unison-ts-install-grammar ()
  "Install tree-sitter grammar for Unison."
  (interactive)
  (unless (assoc 'unison treesit-language-source-alist)
    (add-to-list 'treesit-language-source-alist
                 '(unison "https://github.com/fmguerreiro/tree-sitter-unison")))
  (condition-case err
      (progn
        (message "Installing Unison grammar...")
        (treesit-install-language-grammar 'unison)
        (message "Unison grammar installed successfully")
        t)
    (error
     (message "Failed to install Unison grammar: %s" (error-message-string err))
     nil)))

(defun unison-ts-ensure-grammar ()
  "Ensure tree-sitter grammar for Unison is installed.
Returns t if grammar is available, nil otherwise."
  (cond
   ((treesit-language-available-p 'unison)
    t)

   (unison-ts--install-prompted
    nil)

   ((eq unison-ts-grammar-install 'auto)
    (setq unison-ts--install-prompted t)
    (unison-ts-install-grammar))

   ((eq unison-ts-grammar-install 'prompt)
    (setq unison-ts--install-prompted t)
    (when (y-or-n-p "Install Unison grammar for syntax highlighting? ")
      (unison-ts-install-grammar)))

   (t
    (message "Unison grammar not found. Set unison-ts-grammar-install to enable auto-install.")
    nil)))

(provide 'unison-ts-install)
;;; unison-ts-install.el ends here
