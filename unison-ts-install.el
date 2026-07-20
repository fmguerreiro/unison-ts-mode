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

(require 'treesit)

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
  "Repository URL for the Unison tree-sitter grammar.
When `unison-ts-grammar-revision' is a raw commit SHA the host must allow
fetching unadvertised objects (`uploadpack.allowReachableSHA1InWant'),
which GitHub does but a self-hosted mirror may not; a branch or tag name
fetches from any host."
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

(defconst unison-ts--grammar-revision-branch "unison-ts-pinned-revision"
  "Local branch name pointed at `unison-ts-grammar-revision' during install.")

(defun unison-ts--run-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY, signaling on a non-zero exit."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil arguments)))
        (unless (eq status 0)
          (error "Git %s failed: %s"
                 (string-join arguments " ")
                 (string-trim (buffer-string))))))))

(defun unison-ts--install-grammar ()
  "Fetch the pinned grammar revision and install it into tree-sitter.
`git clone -b' resolves only branch and tag names, never a raw commit
SHA, so this fetches the revision into a temporary directory and points
a local branch at it before registering that branch as the source.  The
branch exists for Emacs 29, which clones the source with `-b'; Emacs 30
runs `git checkout' in place and would take the SHA directly."
  (if (null unison-ts-grammar-revision)
      (let ((treesit-language-source-alist
             (list (list 'unison unison-ts-grammar-repository))))
        (treesit-install-language-grammar 'unison))
    (let ((fetch-directory (make-temp-file "unison-ts-grammar-" t)))
      (unwind-protect
          (progn
            ;; Shallow-fetch just the pinned commit; the full history is
            ;; ~20x the download for one commit.
            (unison-ts--run-git fetch-directory "init" "--quiet")
            (unison-ts--run-git fetch-directory "fetch" "--quiet" "--depth" "1"
                                unison-ts-grammar-repository
                                unison-ts-grammar-revision)
            (unison-ts--run-git fetch-directory "branch"
                                unison-ts--grammar-revision-branch "FETCH_HEAD")
            (let ((treesit-language-source-alist
                   (list (list 'unison fetch-directory
                               unison-ts--grammar-revision-branch))))
              (treesit-install-language-grammar 'unison)))
        (delete-directory fetch-directory t)))))

(defun unison-ts-install-grammar ()
  "Install tree-sitter grammar for Unison.
Return t if the grammar is available afterward, nil otherwise.  Emacs
demotes tree-sitter install failures to warnings, so success is
confirmed with `treesit-language-available-p' rather than by trusting
that the install call signaled or returned non-nil."
  (interactive)
  (message "Installing Unison grammar...")
  (let ((install-error
         (condition-case err
             (progn (unison-ts--install-grammar) nil)
           (error (error-message-string err)))))
    (if (treesit-language-available-p 'unison)
        (progn
          (message "Unison grammar installed successfully")
          t)
      (display-warning
       'unison-ts
       (format "Failed to install Unison grammar from %s%s%s"
               unison-ts-grammar-repository
               (if unison-ts-grammar-revision
                   (format " (revision %s)" unison-ts-grammar-revision)
                 "")
               (if install-error
                   (format ": %s" install-error)
                 "; see the tree-sitter error in the *Warnings* buffer"))
       :error)
      nil)))

(defun unison-ts-ensure-grammar ()
  "Ensure tree-sitter grammar for Unison is installed.
Return t if the grammar is available, nil if it is unavailable and
installation was declined, disabled, or failed."
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
