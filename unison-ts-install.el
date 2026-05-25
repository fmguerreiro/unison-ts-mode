;;; unison-ts-install.el --- Grammar installation for unison-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defcustom unison-ts-grammar-repository "https://github.com/kylegoetz/tree-sitter-unison"
  "Repository URL for the Unison tree-sitter grammar."
  :type 'string
  :group 'unison-ts)

(defcustom unison-ts-grammar-revision "662bf52b966108cf299090a238cd6abfb65d5170"
  "Git revision (branch, tag, or commit) for the grammar.
If nil, uses the default branch.
Pinned to the last upstream commit whose generated parser runs on the
older tree-sitter runtimes bundled with Emacs 29 and some Emacs 30
builds.  Commit b2ae57b (the child of this pin) and later were
regenerated with a newer tree-sitter CLI and misparse `let'/`handle'
expressions on those runtimes; bump only after verifying a newer
revision against Emacs 29 and 30."
  :type '(choice (const :tag "Default branch" nil)
                 (string :tag "Branch/tag/commit"))
  :group 'unison-ts)

(defvar unison-ts--install-prompted nil
  "Whether we've already prompted for installation this session.")

(defun unison-ts-install-grammar ()
  "Install tree-sitter grammar for Unison.
Signal an error if cloning, checkout, or building the grammar fails;
the grammar is pinned to a specific revision and a partial install
would silently degrade syntax highlighting."
  (interactive)
  (unless (assoc 'unison treesit-language-source-alist)
    (add-to-list 'treesit-language-source-alist
                 (if unison-ts-grammar-revision
                     (list 'unison unison-ts-grammar-repository unison-ts-grammar-revision)
                   (list 'unison unison-ts-grammar-repository))))
  (message "Installing Unison grammar...")
  (condition-case err
      (progn
        (treesit-install-language-grammar 'unison)
        (message "Unison grammar installed successfully")
        t)
    (error
     (signal (car err)
             (list (format "Failed to install Unison grammar from %s%s: %s"
                           unison-ts-grammar-repository
                           (if unison-ts-grammar-revision
                               (format " (revision %s)" unison-ts-grammar-revision)
                             "")
                           (error-message-string err)))))))

(defun unison-ts-ensure-grammar ()
  "Ensure tree-sitter grammar for Unison is installed.
Return t if grammar is available, nil if installation was declined or
disabled.  Signal an error if an attempted installation fails."
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

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-install.el ends here
