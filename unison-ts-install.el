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

(defvar unison-ts--install-declined nil
  "Non-nil once the user declined the install prompt this session.
Set only on an explicit \"no\".  Consulted in every mode, so it also
suppresses a later auto-install this session; a failed install does not
set it, leaving the user free to be asked again or to retry with
\\[unison-ts-install-grammar].")

(defvar unison-ts--install-failed nil
  "Non-nil once an automatic install attempt failed this session.
Only consulted when `unison-ts-grammar-install' is `auto'.  It stops an
unattended clone+compile from repeating on every .u buffer opened
against a persistently broken toolchain; retry with
\\[unison-ts-install-grammar] after fixing the toolchain.  Never reset:
once install succeeds the availability check in `unison-ts-ensure-grammar'
short-circuits, so clearing this flag would be dead code.")

(defconst unison-ts--grammar-revision-branch "unison-ts-pinned-revision"
  "Local branch name pointed at `unison-ts-grammar-revision' during install.")

(defun unison-ts--run-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY, signaling on a non-zero exit."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil arguments)))
        (unless (eq status 0)
          (error "git %s failed: %s"
                 (string-join arguments " ")
                 (string-trim (buffer-string))))))))

(defun unison-ts--install-grammar ()
  "Clone the pinned grammar and install it into tree-sitter.
Emacs hands `unison-ts-grammar-revision' to `git clone -b', which
resolves only branch and tag names, so the raw commit SHA the grammar
is pinned to can never be cloned that way.  Clone the repository into a
temporary directory, point a local branch at the revision, and register
that directory and branch as the grammar source.  Emacs 30 checks the
branch out in place; Emacs 29 clones it with `-b'; both land on the
pinned commit.

With no revision there is nothing to pin, so treesit clones the
repository's default branch directly."
  (if (null unison-ts-grammar-revision)
      (let ((treesit-language-source-alist
             (list (list 'unison unison-ts-grammar-repository))))
        (treesit-install-language-grammar 'unison))
    (let ((clone-directory (make-temp-file "unison-ts-grammar-" t)))
      (unwind-protect
          (progn
            ;; Full clone, not --depth 1: the pinned revision predates the
            ;; default-branch tip, so a shallow clone would not contain the
            ;; object `git branch' needs to point at.
            (unison-ts--run-git clone-directory
                                "clone" unison-ts-grammar-repository ".")
            (unison-ts--run-git clone-directory "branch"
                                unison-ts--grammar-revision-branch
                                unison-ts-grammar-revision)
            (let ((treesit-language-source-alist
                   (list (list 'unison clone-directory
                               unison-ts--grammar-revision-branch))))
              (treesit-install-language-grammar 'unison)))
        (delete-directory clone-directory t)))))

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
      (message
       (concat "Failed to install Unison grammar from %s%s%s.  "
               "Retry with M-x unison-ts-install-grammar after fixing "
               "the toolchain.")
               unison-ts-grammar-repository
               (if unison-ts-grammar-revision
                   (format " (revision %s)" unison-ts-grammar-revision)
                 "")
               (if install-error
                   (format ": %s" install-error)
                 "; see the tree-sitter error in the *Warnings* buffer"))
      nil)))

(defun unison-ts-ensure-grammar ()
  "Ensure tree-sitter grammar for Unison is installed.
Return t if the grammar is available, nil if it is unavailable and
installation was declined, disabled, or failed."
  (cond
   ((treesit-language-available-p 'unison)
    t)

   (unison-ts--install-declined
    nil)

   ((eq unison-ts-grammar-install 'auto)
    ;; See `unison-ts--install-failed' for why a failure latches here.
    (unless unison-ts--install-failed
      (or (unison-ts-install-grammar)
          (progn (setq unison-ts--install-failed t) nil))))

   ((eq unison-ts-grammar-install 'prompt)
    ;; Only an explicit decline latches; see `unison-ts--install-declined'.
    (if (y-or-n-p "Install Unison grammar for syntax highlighting? ")
        (unison-ts-install-grammar)
      (setq unison-ts--install-declined t)
      nil))

   (t
    (message "Unison grammar not found. Set unison-ts-grammar-install to enable auto-install.")
    nil)))

(provide 'unison-ts-install)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-install.el ends here
