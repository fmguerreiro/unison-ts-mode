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

(defcustom unison-ts-grammar-revision "bd385224a50baa039d0390fc749be0a0a3071b75"
  "Git revision (branch, tag, or commit) for the grammar.
If nil, uses the default branch.
Pinned to upstream commit bd38522 (grammar 3.0.0).  Since 3.0.0,
keywords and the `=', `:', `->' and `|' delimiters are anonymous nodes,
so font-lock and indent rules match them as strings.  Revisions before
28be881 lex a trailing `--' as a symbolic operator and the comment text
as identifiers, derailing the parse for the rest of the file.
The Emacs 29 CI jobs are the gate on any bump, and the revision is
repeated in README.md and CLAUDE.md."
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
a successful install short-circuits `unison-ts-ensure-grammar' before
this flag is checked again.")

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
runs `git checkout' in place and would take the SHA directly.

With no revision there is nothing to pin, so treesit clones the
repository's default branch directly."
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
       (format (concat "Failed to install Unison grammar from %s%s%s.  "
                       "Retry with M-x unison-ts-install-grammar after "
                       "fixing the toolchain.")
               unison-ts-grammar-repository
               (if unison-ts-grammar-revision
                   (format " (revision %s)" unison-ts-grammar-revision)
                 "")
               (if install-error
                   (format ": %s" install-error)
                 "; see the tree-sitter error in the *Warnings* buffer"))
       :error)
      nil)))

(defun unison-ts--grammar-compatible-p ()
  "Return non-nil for a compatible grammar, otherwise warn how to rebuild it."
  (condition-case nil
      (progn
        (treesit-query-compile
         'unison
         '((if_block) (then_block) (else_block)
           (pattern (var_or_nullary_ctor)) (term_definition "="))
         t)
        t)
    (treesit-query-error
     (display-warning
      'unison-ts
      (concat "The installed Unison grammar is incompatible with this mode.  "
              "Run M-x unison-ts-install-grammar to rebuild it.  If the warning persists, "
              "restart Emacs and check treesit-extra-load-path for an older grammar.")
      :warning)
     nil)))

(defun unison-ts-ensure-grammar ()
  "Return non-nil for a compatible grammar, installing one when configured."
  (cond
   ((treesit-language-available-p 'unison)
    (unison-ts--grammar-compatible-p))

   ;; Must precede the mode arms so a decline suppresses a later
   ;; auto-install too; see `unison-ts--install-declined'.
   (unison-ts--install-declined
    nil)

   ((eq unison-ts-grammar-install 'auto)
    ;; See `unison-ts--install-failed' for why a failure latches here.
    (unless unison-ts--install-failed
      (or (and (unison-ts-install-grammar)
               (unison-ts--grammar-compatible-p))
          (progn (setq unison-ts--install-failed t) nil))))

   ((eq unison-ts-grammar-install 'prompt)
    ;; Only an explicit decline latches; see `unison-ts--install-declined'.
    (if (y-or-n-p "Install Unison grammar for syntax highlighting? ")
        (and (unison-ts-install-grammar)
             (unison-ts--grammar-compatible-p))
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
