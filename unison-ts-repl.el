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

(defvar unison-ts-repl--buffers (make-hash-table :test 'equal :weakness 'value)
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

(defun unison-ts-output--sentinel (proc _event)
  "Process sentinel for UCM output.
Auto-closes buffer on success after `unison-ts-output-auto-close' seconds."
  (when (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc))
             unison-ts-output-auto-close)
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (run-with-timer unison-ts-output-auto-close nil
                        (lambda ()
                          (when (buffer-live-p buf)
                            (delete-windows-on buf)
                            (kill-buffer buf))))))))

(defun unison-ts--run-command (command &optional prompt-arg)
  "Run UCM COMMAND in a compilation buffer.
If PROMPT-ARG is non-nil, prompt for an argument to append."
  (unison-ts--ensure-ucm)
  (let* ((arg (when prompt-arg
                (read-string (format "%s: " prompt-arg))))
         (full-command (if arg
                           (format "%s %s" command arg)
                         command))
         (default-directory (unison-ts-project-root))
         (compile-command (format "%s --noinput -c '%s'"
                                  unison-ts-ucm-executable
                                  full-command)))
    (compilation-start compile-command 'unison-ts-output-mode
                       (lambda (_) "*unison-output*"))
    (with-current-buffer "*unison-output*"
      (set-process-sentinel (get-buffer-process (current-buffer))
                            #'unison-ts-output--sentinel))))

;;;###autoload
(defun unison-ts-add ()
  "Add definitions from the current file to the codebase."
  (interactive)
  (when buffer-file-name
    (unison-ts--run-command (format "add %s" (file-relative-name buffer-file-name
                                                                  (unison-ts-project-root))))))

;;;###autoload
(defun unison-ts-update ()
  "Update existing definitions in the codebase."
  (interactive)
  (unison-ts--run-command "update"))

;;;###autoload
(defun unison-ts-test ()
  "Run tests matching a pattern."
  (interactive)
  (unison-ts--run-command "test" "Test pattern (empty for all)"))

;;;###autoload
(defun unison-ts-run ()
  "Run a term from the codebase."
  (interactive)
  (unison-ts--run-command "run" "Term to run"))

;;;###autoload
(defun unison-ts-watch ()
  "Watch the current file for changes."
  (interactive)
  (when buffer-file-name
    (let* ((default-directory (unison-ts-project-root))
           (rel-path (file-relative-name buffer-file-name default-directory)))
      (unison-ts--run-command (format "watch %s" rel-path)))))

;;;###autoload
(defun unison-ts-load ()
  "Load the current scratch file."
  (interactive)
  (when buffer-file-name
    (let* ((default-directory (unison-ts-project-root))
           (rel-path (file-relative-name buffer-file-name default-directory)))
      (unison-ts--run-command (format "load %s" rel-path)))))

(easy-menu-define unison-ts-ucm-menu nil
  "Menu for UCM commands."
  '("UCM"
    ["Open REPL" unison-ts-repl]
    "---"
    ["Add" unison-ts-add :active buffer-file-name]
    ["Update" unison-ts-update]
    ["Load" unison-ts-load :active buffer-file-name]
    ["Watch" unison-ts-watch :active buffer-file-name]
    "---"
    ["Test..." unison-ts-test]
    ["Run..." unison-ts-run]))

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
