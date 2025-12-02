;;; unison-ts-repl.el --- UCM integration for unison-ts-mode -*- lexical-binding: t; -*-

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
;; UCM (Unison Codebase Manager) integration for unison-ts-mode.
;; Provides REPL interaction and command execution.
;;
;;; Code:

(require 'comint)
(require 'compile)
(require 'project)

(defgroup unison-ts-repl nil
  "UCM integration for Unison."
  :group 'unison-ts
  :prefix "unison-ts-")

(defcustom unison-ts-ucm-executable "ucm"
  "Path to the UCM executable."
  :type 'string
  :group 'unison-ts-repl)

(defcustom unison-ts-output-auto-close 2
  "Seconds to wait before closing output buffer on success.
Set to nil to disable auto-close."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disable" nil))
  :group 'unison-ts-repl)

(defun unison-ts-project-root ()
  "Find Unison project root by looking for .unison directory.
Falls back to `project-root' or `default-directory'."
  (or (locate-dominating-file default-directory ".unison")
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun unison-ts-project-name ()
  "Return the name of the current Unison project."
  (file-name-nondirectory (directory-file-name (unison-ts-project-root))))

(defun unison-ts--ensure-ucm ()
  "Ensure UCM executable is available.
Signals an error if UCM is not found."
  (unless (executable-find unison-ts-ucm-executable)
    (user-error "UCM not found. Install from https://unison-lang.org")))

(defvar unison-ts-repl--buffers (make-hash-table :test 'equal)
  "Hash table mapping project roots to their UCM REPL buffers.")

(defvar unison-ts-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-o") #'comint-clear-buffer)
    map)
  "Keymap for `unison-ts-repl-mode'.")

(define-derived-mode unison-ts-repl-mode comint-mode "UCM"
  "Major mode for interacting with UCM."
  :group 'unison-ts-repl
  (setq-local comint-prompt-regexp "^[^>\n]*> ")
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil))

(defun unison-ts-repl--buffer-name ()
  "Return the REPL buffer name for the current project."
  (format "*ucm: %s*" (unison-ts-project-name)))

(defun unison-ts-repl--get-buffer ()
  "Get or create the UCM REPL buffer for the current project.
Returns nil if UCM process is not running."
  (let* ((root (unison-ts-project-root))
         (buf (gethash root unison-ts-repl--buffers)))
    (when (and buf (buffer-live-p buf)
               (get-buffer-process buf))
      buf)))

(defun unison-ts-repl--start ()
  "Start UCM REPL for the current project."
  (unison-ts--ensure-ucm)
  (let* ((root (unison-ts-project-root))
         (default-directory root)
         (buf-name (unison-ts-repl--buffer-name))
         (buf (make-comint-in-buffer "ucm" buf-name unison-ts-ucm-executable nil)))
    (with-current-buffer buf
      (unison-ts-repl-mode))
    (puthash root buf unison-ts-repl--buffers)
    buf))

;;;###autoload
(defun unison-ts-repl ()
  "Switch to UCM REPL buffer, starting UCM if needed."
  (interactive)
  (let ((buf (or (unison-ts-repl--get-buffer)
                 (unison-ts-repl--start))))
    (pop-to-buffer buf)))

(defvar unison-ts-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    map)
  "Keymap for `unison-ts-output-mode'.")

(define-compilation-mode unison-ts-output-mode "UCM-Output"
  "Major mode for UCM command output."
  (setq-local compilation-error-regexp-alist
              '(("^\\s-*\\([0-9]+\\) \\|" nil 1)))
  (setq-local compilation-disable-input t))

(defvar unison-ts-output--timer nil
  "Timer for auto-closing output buffer.")

(defun unison-ts-output--sentinel (proc _event)
  "Process sentinel for UCM output.
Auto-closes buffer on success after `unison-ts-output-auto-close' seconds."
  (when (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc))
             unison-ts-output-auto-close)
    (when unison-ts-output--timer
      (cancel-timer unison-ts-output--timer))
    (setq unison-ts-output--timer
          (run-with-timer unison-ts-output-auto-close nil
                          (lambda ()
                            (when-let ((buf (process-buffer proc)))
                              (when (buffer-live-p buf)
                                (delete-windows-on buf)
                                (kill-buffer buf))))))))

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
