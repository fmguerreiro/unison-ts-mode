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
(require 'json)
(require 'project)
(require 'treesit)
(require 'seq)
(require 'url)

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

;;; UCM API Integration
;;
;; When UCM runs in headless mode (e.g., for LSP), it exposes an HTTP API.
;; These functions allow sending commands via that API instead of spawning
;; a new UCM process, avoiding codebase lock conflicts.

(defcustom unison-ts-api-port 5858
  "Port for the UCM codebase server API.
This is the port UCM headless exposes for HTTP API requests.
Note: This is different from the LSP port (default 5757)."
  :type 'integer
  :group 'unison-ts-repl)

(defcustom unison-ts-api-token nil
  "Authentication token for UCM API requests.
If nil, no token is sent.  Set this if UCM was started with --token."
  :type '(choice (string :tag "Token")
                 (const :tag "None" nil))
  :group 'unison-ts-repl)

(defcustom unison-ts-api-host "localhost"
  "Host for the UCM codebase server API."
  :type 'string
  :group 'unison-ts-repl)

(defcustom unison-ts-lsp-port 5757
  "Port for the UCM LSP server.
This is the default port UCM uses for LSP (language server protocol)."
  :type 'integer
  :group 'unison-ts-repl)

(defun unison-ts-api--port-open-p (port)
  "Return non-nil if PORT is accepting connections on localhost."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "unison-port-check"
                   :host unison-ts-api-host
                   :service port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun unison-ts-api--lsp-running-p ()
  "Return non-nil if UCM LSP server is running."
  (unison-ts-api--port-open-p unison-ts-lsp-port))

(defun unison-ts-api--headless-conflict-p ()
  "Return non-nil if there's a potential conflict with headless UCM.
This checks if the LSP port is in use, indicating eglot/lsp-mode
started a headless UCM that would conflict with a new REPL instance."
  (unison-ts-api--lsp-running-p))

(defun unison-ts-api--server-running-p ()
  "Return non-nil if a UCM headless server is accepting connections."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "unison-api-check"
                   :host unison-ts-api-host
                   :service unison-ts-api-port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun unison-ts-api--get-endpoint ()
  "Return the UCM API base endpoint URL."
  (let ((base (format "http://%s:%d" unison-ts-api-host unison-ts-api-port)))
    (if unison-ts-api-token
        (format "%s?token=%s" base unison-ts-api-token)
      base)))

(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

(defun unison-ts-api--call (command)
  "Send COMMAND to UCM via the HTTP API.
Returns the response body as a string, or signals an error."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (json-encode `((command . ,command))))
        (endpoint (format "%s/api/ucm/command" (unison-ts-api--get-endpoint))))
    (let ((buffer (url-retrieve-synchronously endpoint t)))
      (when buffer
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer buffer))))))

(defun unison-ts--send-command (command)
  "Send COMMAND to UCM, preferring API if headless server is running.
Falls back to REPL if no headless server is available."
  (if (unison-ts-api--server-running-p)
      (unison-ts-api--call command)
    (unison-ts--send-to-repl command)))

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
    (user-error "UCM not found.  Install from https://unison-lang.org")))

(defvar unison-ts-repl--buffers (make-hash-table :test 'equal :weakness 'value)
  "Hash table mapping project roots to their UCM REPL buffers.")

(defun unison-ts--external-ucm-pids ()
  "Return list of external UCM process PIDs (not managed by Emacs)."
  (let ((emacs-ucm-pids (delq nil
                              (mapcar (lambda (buf)
                                        (when-let ((proc (get-buffer-process buf)))
                                          (process-id proc)))
                                      (hash-table-values unison-ts-repl--buffers))))
        (all-pids nil))
    (with-temp-buffer
      (when (zerop (call-process "pgrep" nil t nil "-x" "ucm"))
        (setq all-pids (mapcar #'string-to-number
                               (split-string (buffer-string) "\n" t)))))
    (seq-difference all-pids emacs-ucm-pids)))

(defun unison-ts--find-ucm-buffer ()
  "Find any buffer running UCM (term, vterm, eshell, shell).
Returns the buffer or nil."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (process-live-p (get-buffer-process buf))
                     (string-match-p "ucm" (buffer-name buf)))))
            (buffer-list)))

(defun unison-ts--kill-external-ucm ()
  "Kill external UCM processes after confirmation."
  (let ((pids (unison-ts--external-ucm-pids)))
    (when pids
      (dolist (pid pids)
        (signal-process pid 'TERM))
      (sit-for 0.5))))

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
  "Start UCM REPL for the current project.
Checks for existing UCM processes and handles conflicts."
  (unison-ts--ensure-ucm)
  (let ((existing-buf (unison-ts--find-ucm-buffer))
        (external-pids (unison-ts--external-ucm-pids))
        (lsp-running (unison-ts-api--lsp-running-p)))
    (cond
     ;; Already have a REPL buffer - just use it
     (existing-buf existing-buf)
     ;; LSP/headless conflict - kill it and start REPL
     ;; This is the common case from GitHub issue #2
     ((and lsp-running external-pids)
      (message "Stopping UCM LSP server to start REPL...")
      (unison-ts--kill-external-ucm)
      (sit-for 1)
      (unison-ts-repl--do-start))
     ;; External UCM running (not LSP) - kill it
     (external-pids
      (message "Stopping external UCM to start REPL...")
      (unison-ts--kill-external-ucm)
      (unison-ts-repl--do-start))
     ;; LSP port open but no UCM process found - something else on that port
     (lsp-running
      (user-error "Port %d is in use (LSP port). Cannot start UCM REPL" unison-ts-lsp-port))
     ;; All clear - start fresh
     (t
      (unison-ts-repl--do-start)))))

(defun unison-ts-repl--do-start ()
  "Actually start the UCM REPL process."
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
PROC is the process.  Auto-close buffer on success after
`unison-ts-output-auto-close' seconds."
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

(defun unison-ts--send-to-repl (command)
  "Send COMMAND to the UCM REPL, starting it if needed."
  (let ((buf (or (unison-ts-repl--get-buffer)
                 (unison-ts-repl--start))))
    (with-current-buffer buf
      (goto-char (point-max))
      (comint-send-string (get-buffer-process buf) (concat command "\n")))
    (display-buffer buf)))

;;;###autoload
(defun unison-ts-add ()
  "Add definitions from the current file to the codebase."
  (interactive)
  (unison-ts--send-to-repl "add"))

;;;###autoload
(defun unison-ts-update ()
  "Update existing definitions in the codebase."
  (interactive)
  (unison-ts--send-to-repl "update"))

;;;###autoload
(defun unison-ts-test ()
  "Run test matching a pattern."
  (interactive)
  (let ((pattern (read-string "Test pattern (empty for all): ")))
    (unison-ts--send-to-repl (if (string-empty-p pattern)
                                  "test"
                                (format "test %s" pattern)))))

;;;###autoload
(defun unison-ts-run ()
  "Run a term from the codebase."
  (interactive)
  (let ((term (read-string "Term to run: ")))
    (unison-ts--send-to-repl (format "run %s" term))))

;;;###autoload
(defun unison-ts-watch ()
  "Watch current file and reload on change."
  (interactive)
  (when buffer-file-name
    (unison-ts--send-to-repl (format "load %s" (file-relative-name
                                                 buffer-file-name
                                                 (unison-ts-project-root))))))

;;;###autoload
(defun unison-ts-load ()
  "Load the current scratch file."
  (interactive)
  (when buffer-file-name
    (unison-ts--send-to-repl (format "load %s" (file-relative-name
                                                 buffer-file-name
                                                 (unison-ts-project-root))))))

(defun unison-ts--send-code-to-repl (code)
  "Send CODE to UCM by writing to a temp file and loading it."
  (let* ((root (unison-ts-project-root))
         (temp-file (expand-file-name ".scratch.u" root)))
    (with-temp-file temp-file
      (insert code))
    (unison-ts--send-to-repl (format "load %s" (file-relative-name temp-file root)))))

;;;###autoload
(defun unison-ts-send-region (start end)
  "Send the region between START and END to the UCM REPL."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region active"))
  (let ((text (buffer-substring-no-properties start end)))
    (unison-ts--send-code-to-repl text)))

(defun unison-ts--definition-node-p (node)
  "Return non-nil if NODE is a Unison definition."
  (member (treesit-node-type node)
          '("term_declaration" "type_declaration" "ability_declaration")))

;;;###autoload
(defun unison-ts-send-definition ()
  "Send the definition at point to the UCM REPL."
  (interactive)
  (let ((node (treesit-node-at (point))))
    (unless node
      (user-error "No tree-sitter node at point"))
    (let ((def-node (treesit-parent-until node #'unison-ts--definition-node-p t)))
      (unless def-node
        (user-error "Point is not within a definition"))
      (let ((text (treesit-node-text def-node t)))
        (unison-ts--send-code-to-repl text)))))

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
