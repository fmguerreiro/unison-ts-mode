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

(defcustom unison-ts-eval-overlay t
  "When non-nil, display eval results as transient inline overlays.
When nil, all overlay logic is a no-op and behavior matches the pre-overlay
default (minibuffer only)."
  :type 'boolean
  :group 'unison-ts-repl)

(defcustom unison-ts-eval-overlay-format " => %s"
  "Format string for inline eval result overlays.
%s is replaced with the result string."
  :type 'string
  :group 'unison-ts-repl)

(defface unison-ts-eval-overlay-face
  '((t :inherit font-lock-comment-face))
  "Face used for inline eval result overlays."
  :group 'unison-ts-repl)

(defvar-local unison-ts--eval-overlay nil
  "Current eval result overlay in this buffer, or nil.")

(defun unison-ts--overlay-clear ()
  "Delete the current eval overlay and remove the pre-command hook."
  (when (overlayp unison-ts--eval-overlay)
    (delete-overlay unison-ts--eval-overlay))
  (setq unison-ts--eval-overlay nil)
  (remove-hook 'pre-command-hook #'unison-ts--overlay-clear t))

(defconst unison-ts--overlay-max-length 120
  "Maximum character length for inline eval result overlays.
Results longer than this are not shown as overlays.")

(defun unison-ts--overlay-show (position result-string)
  "Display RESULT-STRING as a transient overlay at POSITION.
POSITION is a buffer position (integer or marker).
Does nothing when `unison-ts-eval-overlay' is nil, when RESULT-STRING
contains a newline, or when it exceeds `unison-ts--overlay-max-length'."
  (when (and unison-ts-eval-overlay
             (not (string-match-p "\n" result-string))
             (<= (length result-string) unison-ts--overlay-max-length))
    (unison-ts--overlay-clear)
    (let* ((text (format unison-ts-eval-overlay-format result-string))
           (overlay (make-overlay position position nil t t)))
      (overlay-put overlay 'after-string
                   (propertize text 'face 'unison-ts-eval-overlay-face))
      (setq unison-ts--eval-overlay overlay)
      (add-hook 'pre-command-hook #'unison-ts--overlay-clear nil t))))

(defconst unison-ts--lsp-startup-max-attempts 100
  "Maximum attempts to wait for LSP server startup (0.1s each = 10s total).")

;;; UCM MCP Client
;;
;; UCM provides an MCP (Model Context Protocol) server that allows sending
;; commands without codebase lock conflicts. This is the preferred method
;; when LSP is running.

(defvar unison-ts-mcp--request-id 0
  "Counter for JSON-RPC request IDs.")

(defun unison-ts-mcp--make-request (method params)
  "Create a JSON-RPC 2.0 request for METHOD with PARAMS."
  (setq unison-ts-mcp--request-id (1+ unison-ts-mcp--request-id))
  `((jsonrpc . "2.0")
    (method . ,method)
    (params . ,params)
    (id . ,unison-ts-mcp--request-id)))

(defun unison-ts-mcp--call (requests &optional callback)
  "Send REQUESTS to ucm mcp asynchronously.
REQUESTS is a list of (method . params) cons cells.
CALLBACK is called with the list of responses when complete.
If CALLBACK is nil, runs synchronously and returns responses."
  (let* ((default-directory (unison-ts-project-root))
         (empty-obj (make-hash-table))
         (init-req (unison-ts-mcp--make-request
                    "initialize"
                    `((protocolVersion . "2024-11-05")
                      (capabilities . ,empty-obj)
                      (clientInfo . ((name . "unison-ts-mode")
                                     (version . "0.1.0"))))))
         (all-requests (cons init-req
                             (mapcar (lambda (r)
                                       (unison-ts-mcp--make-request (car r) (cdr r)))
                                     requests)))
         (input (mapconcat #'json-encode all-requests "\n")))
    (if callback
        ;; Async mode
        (let* ((output-buffer (generate-new-buffer " *ucm-mcp-output*"))
               (proc (make-process
                      :name "ucm-mcp"
                      :buffer output-buffer
                      :command (list unison-ts-ucm-executable "mcp")
                      :connection-type 'pipe
                      :sentinel (lambda (proc _event)
                                  (when (memq (process-status proc) '(exit signal))
                                    (let ((responses (unison-ts-mcp--parse-output output-buffer)))
                                      (kill-buffer output-buffer)
                                      (funcall callback responses))))
                      :filter (lambda (proc string)
                                (with-current-buffer (process-buffer proc)
                                  (goto-char (point-max))
                                  (insert string))))))
          (process-send-string proc input)
          (process-send-string proc "\n")
          (process-send-eof proc)
          nil)
      ;; Sync mode (for REPL)
      (let ((output (with-temp-buffer
                      (let ((exit-code (call-process-region
                                        input nil
                                        unison-ts-ucm-executable
                                        nil t nil
                                        "mcp")))
                        (unless (zerop exit-code)
                          (user-error "UCM MCP failed with exit code %d: %s"
                                      exit-code (buffer-string)))
                        (buffer-string)))))
        (unison-ts-mcp--parse-output-string output)))))

(defun unison-ts-mcp--parse-output (buffer)
  "Parse MCP responses from BUFFER, skipping init response."
  (with-current-buffer buffer
    (unison-ts-mcp--parse-output-string (buffer-string))))

(defun unison-ts-mcp--parse-output-string (output)
  "Parse MCP responses from OUTPUT string, skipping init response."
  (let* ((lines (split-string output "\n" t))
         (responses (mapcar (lambda (line)
                              (condition-case err
                                  (json-read-from-string line)
                                (error
                                 (lwarn 'unison-ts-mcp :error
                                        "Failed to parse MCP response: %S\nLine: %s"
                                        err line)
                                 nil)))
                            lines)))
    (cdr responses)))

(defun unison-ts-mcp--call-tool (tool-name arguments &optional callback)
  "Call MCP tool TOOL-NAME with ARGUMENTS.
If CALLBACK is provided, runs asynchronously and calls CALLBACK with result.
Otherwise runs synchronously and returns result."
  (let ((request `(("tools/call" . ((name . ,tool-name)
                                    (arguments . ,(or arguments (make-hash-table))))))))
    (if callback
        (unison-ts-mcp--call
         request
         (lambda (responses)
           (funcall callback (alist-get 'result (car responses)))))
      (let* ((responses (unison-ts-mcp--call request))
             (response (car responses)))
        (alist-get 'result response)))))

(defun unison-ts-mcp--get-project-context ()
  "Get the current project context (name and branch) via MCP."
  (let ((result (unison-ts-mcp--call-tool "get-current-project-context" nil)))
    (when result
      (let* ((content-array (alist-get 'content result))
             (content (if (vectorp content-array)
                          (aref content-array 0)
                        (car content-array))))
        (when (and content (equal (alist-get 'type content) "text"))
          (json-read-from-string (alist-get 'text content)))))))

(defun unison-ts-mcp--with-project-context (func)
  "Call FUNC with project-name and branch-name from current context.
FUNC should accept two arguments: project-name and branch-name.
Signals an error if no project context is found."
  (let ((ctx (unison-ts-mcp--get-project-context)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (funcall func
             (alist-get 'projectName ctx)
             (alist-get 'branchName ctx))))

(defun unison-ts-mcp--make-project-context (project-name branch-name)
  "Create a projectContext alist from PROJECT-NAME and BRANCH-NAME."
  `((projectContext . ((projectName . ,project-name)
                       (branchName . ,branch-name)))))

(defun unison-ts-mcp--update-definitions (code)
  "Update definitions with CODE via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "update-definitions"
      (append (unison-ts-mcp--make-project-context project-name branch-name)
              `((code . ((text . ,code)))))))))

(defun unison-ts-mcp--run-tests ()
  "Run tests in current project via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "run-tests"
      (unison-ts-mcp--make-project-context project-name branch-name)))))

(defun unison-ts-mcp--run (definition)
  "Run DEFINITION in current project via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "run"
      (append (unison-ts-mcp--make-project-context project-name branch-name)
              `((definition . ,definition)))))))

;;; UCM Headless Detection

(defcustom unison-ts-api-host "localhost"
  "Host for the UCM codebase server API."
  :type 'string
  :group 'unison-ts-repl)

(defcustom unison-ts-lsp-port 5757
  "Port for the UCM LSP server.
This is the default port UCM uses for LSP (language server protocol).
The environment variable UNISON_LSP_PORT overrides this value at
runtime via `unison-ts--resolve-lsp-port'."
  :type 'integer
  :group 'unison-ts-repl)

(defun unison-ts--resolve-lsp-port ()
  "Return the effective UCM LSP port.
Uses the UNISON_LSP_PORT env var when set, falling back to the
`unison-ts-lsp-port' defcustom."
  (if-let ((env (getenv "UNISON_LSP_PORT")))
      (string-to-number env)
    unison-ts-lsp-port))

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
  (unison-ts-api--port-open-p (unison-ts--resolve-lsp-port)))

(defun unison-ts-project-root ()
  "Find the project root for the current buffer.
Prefers `project-current'; falls back to `default-directory'."
  (or (when-let ((proj (project-current)))
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

;;; Comint-based REPL (when no headless UCM is running)

(defvar unison-ts-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-o") #'comint-clear-buffer)
    map)
  "Keymap for `unison-ts-repl-mode'.")

(define-derived-mode unison-ts-repl-mode comint-mode "UCM"
  "Major mode for interacting with UCM via subprocess."
  :group 'unison-ts-repl
  (setq-local comint-prompt-regexp "^[^>\n]*> ")
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil))

;;; MCP-based REPL (when headless UCM is running)
;;
;; This mode provides a REPL-like interface that sends commands via MCP
;; to avoid codebase lock conflicts with the running headless UCM.
;;
;; Supported commands:
;;   > <code>           - Evaluate a Unison expression (watch/typecheck)
;;   add <code>         - Add/update definitions
;;   test [namespace]   - Run tests
;;   run <name> [args]  - Run a function
;;   view <names>       - View definition source
;;   find <query>       - Search by name
;;   find : <type>      - Search by type
;;   docs <name>        - Show documentation
;;   help               - Show available commands

(defvar-local unison-ts-mcp-repl--input-start nil
  "Marker for the start of user input in MCP REPL buffer.")

(defvar-local unison-ts-mcp-repl--project-root nil
  "Project root associated with this MCP REPL buffer.")

(defvar-local unison-ts-mcp-repl--history-index -1
  "Current position in history ring. -1 means not navigating history.")

(defvar-local unison-ts-mcp-repl--saved-input nil
  "Input saved before starting history navigation.")

(defun unison-ts-mcp-repl--replace-input (text)
  "Replace current input with TEXT."
  (let ((inhibit-read-only t))
    (delete-region (marker-position unison-ts-mcp-repl--input-start) (point-max))
    (goto-char (point-max))
    (insert text)))

(defun unison-ts-mcp-repl-previous-input ()
  "Navigate to previous input in history."
  (interactive)
  (when (and comint-input-ring (not (ring-empty-p comint-input-ring)))
    (let ((ring-len (ring-length comint-input-ring)))
      ;; Save current input when starting navigation
      (when (< unison-ts-mcp-repl--history-index 0)
        (setq unison-ts-mcp-repl--saved-input (unison-ts-mcp-repl--get-input)))
      ;; Move back in history
      (when (< unison-ts-mcp-repl--history-index (1- ring-len))
        (setq unison-ts-mcp-repl--history-index (1+ unison-ts-mcp-repl--history-index))
        (unison-ts-mcp-repl--replace-input
         (ring-ref comint-input-ring unison-ts-mcp-repl--history-index))))))

(defun unison-ts-mcp-repl-next-input ()
  "Navigate to next input in history."
  (interactive)
  (when (>= unison-ts-mcp-repl--history-index 0)
    (setq unison-ts-mcp-repl--history-index (1- unison-ts-mcp-repl--history-index))
    (unison-ts-mcp-repl--replace-input
     (if (< unison-ts-mcp-repl--history-index 0)
         (or unison-ts-mcp-repl--saved-input "")
       (ring-ref comint-input-ring unison-ts-mcp-repl--history-index)))))

(defvar unison-ts-mcp-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unison-ts-mcp-repl-send)
    (define-key map (kbd "C-c M-o") #'unison-ts-mcp-repl-clear)
    (define-key map (kbd "C-a") #'unison-ts-mcp-repl-bol)
    (define-key map (kbd "M-p") #'unison-ts-mcp-repl-previous-input)
    (define-key map (kbd "M-n") #'unison-ts-mcp-repl-next-input)
    map)
  "Keymap for `unison-ts-mcp-repl-mode'.")

(defun unison-ts-mcp-repl--input-sender (_proc input)
  "Send INPUT to UCM via MCP (ignores PROC since we don't use subprocess)."
  (let ((parsed (unison-ts-mcp-repl--parse-command input)))
    (when parsed
      (condition-case err
          (unison-ts-mcp-repl--execute-async
           (car parsed)
           (cdr parsed)
           (lambda (result)
             (unison-ts-mcp-repl--insert-response result)
             (unison-ts-mcp-repl--insert-prompt)))
        (error
         (unison-ts-mcp-repl--insert-error (error-message-string err))
         (unison-ts-mcp-repl--insert-prompt))))))

(define-derived-mode unison-ts-mcp-repl-mode fundamental-mode "UCM-MCP"
  "Major mode for interacting with UCM via MCP protocol.
This mode sends commands to UCM via MCP, avoiding codebase lock
conflicts with headless UCM (LSP).

Key bindings:
\\{unison-ts-mcp-repl-mode-map}

Type `help' in the REPL for available commands."
  :group 'unison-ts-repl
  (setq-local unison-ts-mcp-repl--input-start (make-marker))
  (setq-local unison-ts-mcp-repl--history-index -1)
  (setq-local unison-ts-mcp-repl--saved-input nil)
  (setq-local comint-input-ring (make-ring comint-input-ring-size)))

(defun unison-ts-mcp-repl--insert-prompt ()
  "Insert the REPL prompt and set up input marker."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "%s> " (or (file-name-nondirectory
                                            (directory-file-name
                                             (or unison-ts-mcp-repl--project-root
                                                 default-directory)))
                                           "ucm"))
                        'face 'comint-highlight-prompt
                        'read-only t
                        'rear-nonsticky t))
    (set-marker unison-ts-mcp-repl--input-start (point))))

(defun unison-ts-mcp-repl-bol ()
  "Move to beginning of line, but after the prompt."
  (interactive)
  (let ((prompt-end (marker-position unison-ts-mcp-repl--input-start)))
    (if (and prompt-end (>= (point) prompt-end)
             (= (line-number-at-pos) (line-number-at-pos prompt-end)))
        (goto-char prompt-end)
      (beginning-of-line))))

(defun unison-ts-mcp-repl--get-input ()
  "Get the current input from the REPL buffer."
  (buffer-substring-no-properties
   (marker-position unison-ts-mcp-repl--input-start)
   (point-max)))

(defconst unison-ts-mcp-repl--help-text
  "UCM MCP REPL Commands:

  > <expr>          Evaluate/typecheck a Unison expression
  add <code>        Add or update definitions
  test [namespace]  Run tests (optionally in a namespace)
  run <name> [args] Execute a function
  view <names>      View definition source code
  find <query>      Search definitions by name
  find : <type>     Search definitions by type signature
  docs <name>       Show documentation for a definition
  help              Show this help message

Examples:
  > 1 + 2
  > List.map (x -> x + 1) [1, 2, 3]
  add myFunc x = x + 1
  test
  run main
  view List.map
  find foldl
  find : [a] -> a
  docs Optional.map
"
  "Help text for MCP REPL commands.")

(defun unison-ts-mcp-repl--parse-command (input)
  "Parse INPUT into (command . args) cons cell."
  (let ((trimmed (string-trim input)))
    (cond
     ;; Empty input
     ((string-empty-p trimmed)
      nil)
     ;; Help
     ((string-match-p "^help\\s-*$" trimmed)
      (cons 'help nil))
     ;; Watch/evaluate: starts with >
     ((string-match "^>\\s-*\\(\\(?:.\\|\n\\)*\\)" trimmed)
      (cons 'watch (match-string 1 trimmed)))
     ;; Add definitions (may span multiple lines)
     ((string-match "^add\\s-+\\(\\(?:.\\|\n\\)*\\)" trimmed)
      (cons 'add (match-string 1 trimmed)))
     ;; Test
     ((string-match "^test\\(?:\\s-+\\(\\(?:.\\|\n\\)*\\)\\)?$" trimmed)
      (cons 'test (match-string 1 trimmed)))
     ;; Run
     ((string-match "^run\\s-+\\(\\S-+\\)\\(?:\\s-+\\(\\(?:.\\|\n\\)*\\)\\)?$" trimmed)
      (cons 'run (cons (match-string 1 trimmed)
                       (when (match-string 2 trimmed)
                         (split-string (match-string 2 trimmed))))))
     ;; View
     ((string-match "^view\\s-+\\(\\(?:.\\|\n\\)*\\)" trimmed)
      (cons 'view (split-string (match-string 1 trimmed))))
     ;; Find by type (find : <type>)
     ((string-match "^find\\s-*:\\s-*\\(\\(?:.\\|\n\\)*\\)" trimmed)
      (cons 'find-type (match-string 1 trimmed)))
     ;; Find by name
     ((string-match "^find\\s-+\\(\\(?:.\\|\n\\)*\\)" trimmed)
      (cons 'find-name (match-string 1 trimmed)))
     ;; Docs
     ((string-match "^docs\\s-+\\(\\S-+\\)" trimmed)
      (cons 'docs (match-string 1 trimmed)))
     ;; Default: treat as code to evaluate
     (t
      (cons 'watch trimmed)))))

(defun unison-ts-mcp-repl--execute-async (command args callback)
  "Execute MCP COMMAND with ARGS asynchronously.
CALLBACK is called with the result string."
  (let ((default-directory (or unison-ts-mcp-repl--project-root
                               default-directory))
        (repl-buffer (current-buffer)))
    (if (eq command 'help)
        (funcall callback unison-ts-mcp-repl--help-text)
      (unison-ts-mcp--with-project-context
       (lambda (project-name branch-name)
         (let* ((ctx (unison-ts-mcp--make-project-context project-name branch-name))
                (wrapped-callback (lambda (result)
                                    (with-current-buffer repl-buffer
                                      (funcall callback (unison-ts-mcp-repl--format-result result))))))
           (pcase command
             ('watch
              (unison-ts-mcp--call-tool
               "typecheck-code"
               (append ctx `((code . ((sourceCode . ,(if (string-prefix-p ">" args)
                                                         args
                                                       (concat "> " args)))))))
               wrapped-callback))
             ('add
              (unison-ts-mcp--call-tool
               "update-definitions"
               (append ctx `((code . ((text . ,args)))))
               wrapped-callback))
             ('test
              (unison-ts-mcp--call-tool
               "run-tests"
               (append ctx (when args `((subnamespace . ,args))))
               wrapped-callback))
             ('run
              (unison-ts-mcp--call-tool
               "run"
               (append ctx `((mainFunctionName . ,(car args))
                             (args . ,(or (cdr args) []))))
               wrapped-callback))
             ('view
              (unison-ts-mcp--call-tool
               "view-definitions"
               (append ctx `((names . ,args)))
               wrapped-callback))
             ('find-name
              (unison-ts-mcp--call-tool
               "search-definitions-by-name"
               (append ctx `((query . ,args)))
               wrapped-callback))
             ('find-type
              (unison-ts-mcp--call-tool
               "search-by-type"
               (append ctx `((query . ,args)))
               wrapped-callback))
             ('docs
              (unison-ts-mcp--call-tool
               "docs"
               (append ctx `((name . ,args)))
               wrapped-callback))
             (_
              (funcall callback (format "Unknown command: %s\nType 'help' for available commands." command))))))))))

(defun unison-ts-mcp-repl--format-result (result)
  "Format MCP RESULT for display in REPL."
  (if result
      (let* ((content-array (alist-get 'content result))
             (content (if (vectorp content-array)
                          (aref content-array 0)
                        (car content-array)))
             (text (alist-get 'text content)))
        (if text
            (let ((parsed (condition-case nil
                              (json-read-from-string text)
                            (error text))))
              (if (and (listp parsed) (not (stringp parsed)))
                  ;; Structured response - deduplicate messages and filter errors from outputs
                  (let* ((errors (unison-ts--dedupe-messages (alist-get 'errorMessages parsed)))
                         (error-keys (mapcar #'string-trim errors))
                         (outputs (seq-filter
                                   (lambda (m)
                                     (and (not (string-match-p "^Loading changes" m))
                                          (not (string-match-p "^No changes found" m))
                                          (not (member (string-trim m) error-keys))))
                                   (unison-ts--dedupe-messages (alist-get 'outputMessages parsed)))))
                    (concat
                     (when (and errors (> (length errors) 0))
                       (concat "Errors:\n"
                               (mapconcat #'identity errors "\n")
                               "\n"))
                     (when (and outputs (> (length outputs) 0))
                       (mapconcat #'identity outputs "\n"))))
                ;; Plain text
                (format "%s" parsed)))
          "(no content)"))
    "(no result)"))

(defun unison-ts-mcp-repl-send ()
  "Send current input to UCM via MCP."
  (interactive)
  (let ((input (string-trim (unison-ts-mcp-repl--get-input))))
    ;; Add to history ring
    (when (and (boundp 'comint-input-ring) comint-input-ring
               (not (string-empty-p input)))
      (ring-insert comint-input-ring input))
    ;; Reset history navigation state
    (setq unison-ts-mcp-repl--history-index -1)
    (setq unison-ts-mcp-repl--saved-input nil)
    ;; Make input read-only and send
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (add-text-properties (marker-position unison-ts-mcp-repl--input-start)
                           (point)
                           '(read-only t)))
    (unison-ts-mcp-repl--input-sender nil input)))

(defun unison-ts-mcp-repl--insert-response (response)
  "Insert MCP RESPONSE into the REPL buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (when (and response (not (string-empty-p (string-trim response))))
      (insert (propertize (string-trim response) 'face 'default) "\n"))))

(defun unison-ts-mcp-repl--insert-error (message)
  "Insert error MESSAGE into the REPL buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "Error: %s\n" message)
                        'face 'error))))

(defun unison-ts-mcp-repl-clear ()
  "Clear the MCP REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (unison-ts-mcp-repl--insert-prompt)))

;;; REPL buffer management

(defun unison-ts-repl--buffer-name ()
  "Return the REPL buffer name for the current project."
  (format "*ucm: %s*" (unison-ts-project-name)))

(defun unison-ts-repl--get-buffer ()
  "Get the UCM MCP REPL buffer for the current project.
Returns nil if no REPL buffer exists or it's not usable."
  (let* ((root (unison-ts-project-root))
         (buf (gethash root unison-ts-repl--buffers)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'unison-ts-mcp-repl-mode)
          buf)))))

;;; Inferior UCM
;;
;; The inferior UCM is the full `ucm' executable running inside an Emacs
;; comint buffer.  It serves both LSP (for eglot/lsp-mode) and MCP (for
;; the REPL) on `unison-ts-lsp-port'.  Running the full UCM rather than
;; `ucm headless' means the user can interact with the UCM TUI inside
;; Emacs without fighting the codebase lock — the lock is held by the
;; same UCM that Emacs is talking to.
;;
;; This addresses gh#8: an Emacs-spawned `ucm headless' previously held
;; the lock and prevented users from running `ucm' (full TUI) elsewhere.

(defcustom unison-ts-inferior-ucm-buffer-name "*ucm*"
  "Buffer name for the inferior UCM (full TUI) process."
  :type 'string
  :group 'unison-ts-repl)

(defvar unison-ts--ucm-process nil
  "Process object for the inferior UCM started by Emacs.")

(defun unison-ts--ucm-on-buffer-kill ()
  "Buffer-local hook: stop the tracked UCM process without killing the buffer.
The buffer is being killed already; recursing through `unison-ts--cleanup-ucm'
would call `kill-buffer' again and blow the stack."
  (when (and unison-ts--ucm-process
             (process-live-p unison-ts--ucm-process))
    (delete-process unison-ts--ucm-process))
  (setq unison-ts--ucm-process nil))

(defun unison-ts--cleanup-ucm ()
  "Tear down the inferior UCM process and buffer started by Emacs."
  (when-let ((buf (get-buffer unison-ts-inferior-ucm-buffer-name)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf)))
  (when (and unison-ts--ucm-process
             (process-live-p unison-ts--ucm-process))
    (delete-process unison-ts--ucm-process))
  (setq unison-ts--ucm-process nil))

(add-hook 'kill-emacs-hook #'unison-ts--cleanup-ucm)

(define-derived-mode unison-ts-inferior-ucm-mode comint-mode "UCM"
  "Major mode for the inferior UCM (Unison Codebase Manager) process.

Inherits from `comint-mode' and enables ANSI colour processing so
UCM's coloured output renders correctly."
  :group 'unison-ts-repl
  (setq-local comint-prompt-regexp "^[^>\n]*>+ ")
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil)
  (ansi-color-for-comint-mode-on)
  (add-hook 'kill-buffer-hook #'unison-ts--ucm-on-buffer-kill nil t))

(defun unison-ts--start-ucm-inferior ()
  "Start the inferior UCM process if not already running.
Returns the inferior UCM buffer, or nil when an external UCM is
already serving `unison-ts-lsp-port'.  Signals an error if the
LSP/MCP port never becomes reachable after starting."
  (unison-ts--ensure-ucm)
  (let* ((buf-name unison-ts-inferior-ucm-buffer-name)
         (existing-buf (get-buffer buf-name))
         (existing-proc (and existing-buf (get-buffer-process existing-buf))))
    (cond
     ((and existing-proc (process-live-p existing-proc))
      existing-buf)
     ((unison-ts-api--lsp-running-p)
      nil)
     (t
      (let* ((default-directory (unison-ts-project-root))
             (buf (get-buffer-create buf-name))
             (port (unison-ts--resolve-lsp-port)))
        ;; Set the mode BEFORE attaching the process.  `make-comint-in-buffer'
        ;; sees `derived-mode-p' is t and skips its own `comint-mode' setup,
        ;; which avoids a race where `kill-all-local-variables' blanks
        ;; `comint-last-output-start' while UCM is already emitting output —
        ;; an `:after' advice on `comint-output-filter' (e.g. Doom's
        ;; `doom--comint-enable-undo-a') would then crash on the nil marker.
        (with-current-buffer buf
          (unison-ts-inferior-ucm-mode))
        (make-comint-in-buffer "ucm" buf unison-ts-ucm-executable nil)
        (setq unison-ts--ucm-process (get-buffer-process buf))
        (unless unison-ts--ucm-process
          (kill-buffer buf)
          (error "make-comint-in-buffer did not attach a process to %s" buf-name))
        (set-process-query-on-exit-flag unison-ts--ucm-process nil)
        (message "Starting UCM...")
        (let ((attempts 0))
          (while (and (< attempts unison-ts--lsp-startup-max-attempts)
                      (not (unison-ts-api--lsp-running-p)))
            (sit-for 0.1)
            (setq attempts (1+ attempts))))
        (unless (unison-ts-api--lsp-running-p)
          (unison-ts--cleanup-ucm)
          (error "UCM started but LSP/MCP port %d did not open" port))
        (message "UCM started on port %d" port)
        buf)))))

;;;###autoload
(defun unison-ts-inferior-ucm ()
  "Switch to the inferior UCM buffer, starting UCM if needed.
The inferior UCM is the full `ucm' TUI running inside Emacs; it
serves both eglot and the MCP REPL.  Errors when an external UCM
is already holding the codebase lock — manage it there instead."
  (interactive)
  (let ((buf (unison-ts--start-ucm-inferior)))
    (unless buf
      (user-error
       "UCM is already running externally on port %d; manage it there"
       (unison-ts--resolve-lsp-port)))
    (pop-to-buffer buf)))

(defun unison-ts-repl--start ()
  "Start UCM REPL for the current project.
Always uses MCP-based REPL.  If no UCM is running, starts the
inferior UCM first.  A single UCM process serves both LSP (eglot)
and the MCP REPL."
  (unison-ts--ensure-ucm)
  (let ((existing-buf (unison-ts-repl--get-buffer)))
    (cond
     (existing-buf existing-buf)
     (t
      (unison-ts--start-ucm-inferior)
      (unison-ts-repl--start-mcp)))))

(defun unison-ts-repl--start-mcp ()
  "Start an MCP-based REPL buffer."
  (let* ((root (unison-ts-project-root))
         (buf-name (unison-ts-repl--buffer-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'unison-ts-mcp-repl-mode)
        (unison-ts-mcp-repl-mode))
      (setq unison-ts-mcp-repl--project-root root)
      (setq default-directory root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "UCM MCP REPL - Connected via MCP protocol\n"
                            'face 'font-lock-comment-face))
        (insert (propertize (format "Project: %s\n"
                                    (file-name-nondirectory
                                     (directory-file-name root)))
                            'face 'font-lock-comment-face))
        (insert (propertize "Type 'help' for available commands.\n\n"
                            'face 'font-lock-comment-face)))
      (unison-ts-mcp-repl--insert-prompt))
    (puthash root buf unison-ts-repl--buffers)
    buf))

;;;###autoload
(defun unison-ts-repl ()
  "Switch to UCM REPL buffer, starting UCM headless if needed.
Uses MCP protocol to communicate with a single UCM headless process,
which also serves LSP for eglot. This avoids codebase lock conflicts
by ensuring only one UCM process runs at a time."
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

(defun unison-ts--send-to-repl--impl (command select)
  "Send COMMAND to the UCM REPL, starting it if needed.
If SELECT is non-nil, use `pop-to-buffer' to switch to the REPL;
otherwise use `display-buffer'."
  (unison-ts--ensure-ucm)
  (let ((buf (or (unison-ts-repl--get-buffer)
                 (unison-ts-repl--start))))
    (with-current-buffer buf
      (if (derived-mode-p 'unison-ts-mcp-repl-mode)
          (progn
            (goto-char (point-max))
            (insert command)
            (unison-ts-mcp-repl-send))
        (goto-char (point-max))
        (comint-send-string (get-buffer-process buf) (concat command "\n"))))
    (funcall (if select #'pop-to-buffer #'display-buffer) buf)))

(defun unison-ts--send-to-repl (command)
  "Send COMMAND to the UCM REPL, starting it if needed."
  (unison-ts--send-to-repl--impl command nil))

(defun unison-ts--send-to-repl-and-go (command)
  "Send COMMAND to the UCM REPL and switch to it."
  (unison-ts--send-to-repl--impl command t))

(defun unison-ts--parse-mcp-output (text)
  "Parse MCP output TEXT which may be JSON-encoded UCM response."
  (condition-case nil
      (let ((parsed (json-read-from-string text)))
        (if (and (listp parsed)
                 (or (alist-get 'errorMessages parsed)
                     (alist-get 'outputMessages parsed)))
            parsed
          text))
    (error text)))

(defun unison-ts--dedupe-messages (messages)
  "Remove duplicate MESSAGES, normalizing whitespace for comparison."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (msg (append messages nil))
      (let ((key (string-trim msg)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push msg result))))
    (nreverse result)))

(defun unison-ts--display-mcp-result (result title at)
  "Display MCP RESULT appropriately based on content.
Short success messages go to minibuffer, errors/long output to a buffer.
AT is a buffer position; when the result is a success, an inline overlay
is shown at that position in addition to the minibuffer message."
  (let ((overlay-position at))
    (if (and result (listp result))
        (let* ((content-array (alist-get 'content result))
               (content (if (vectorp content-array)
                            (aref content-array 0)
                          (car content-array)))
               (text (alist-get 'text content))
               (parsed (when text (unison-ts--parse-mcp-output text))))
          (if (and (listp parsed) (not (stringp parsed)))
              (let* ((errors (unison-ts--dedupe-messages (alist-get 'errorMessages parsed)))
                     (error-keys (mapcar #'string-trim errors))
                     (outputs (seq-filter
                               (lambda (msg)
                                 (and (not (string-match-p "^Loading changes" msg))
                                      (not (string-match-p "^No changes found" msg))
                                      ;; Filter misleading "Run `update`" - MCP already commits
                                      (not (string-match-p "Run `update` to apply" msg))
                                      (not (member (string-trim msg) error-keys))))
                               (unison-ts--dedupe-messages (alist-get 'outputMessages parsed)))))
                (if (= (length errors) 0)
                    ;; Success → minibuffer + optional overlay
                    (let ((output-str (string-join outputs " ")))
                      (if (string-match-p "^\\+" output-str)
                          ;; Definitions were added - show what was added
                          (message "UCM: Added definitions. %s" output-str)
                        (message "UCM: %s" output-str))
                      (unison-ts--overlay-show overlay-position output-str))
                  ;; Errors → buffer
                  (unison-ts--display-in-buffer title errors outputs)))
            ;; Non-parsed output → buffer
            (unison-ts--display-in-buffer title nil (list (format "%s" parsed)))))
      ;; Fallback → buffer
      (unison-ts--display-in-buffer title nil (list (format "%S" result))))))

(defun unison-ts--display-in-buffer (title errors outputs)
  "Display ERRORS and OUTPUTS in a *UCM: TITLE* buffer.
Uses `pop-to-buffer' for errors to ensure visibility."
  (let ((buf (get-buffer-create (format "*UCM: %s*" title)))
        (has-errors (and errors (> (length errors) 0))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when has-errors
          (insert "⚠️ Errors:\n")
          (seq-do (lambda (msg) (insert msg "\n")) errors)
          (insert "\n"))
        (when outputs
          (seq-do (lambda (msg) (insert msg "\n")) outputs))
        (goto-char (point-min))
        (special-mode)))
    (if has-errors
        (progn
          (pop-to-buffer buf)
          (message "UCM: Errors occurred - see buffer"))
      (display-buffer buf))))

(defun unison-ts--update-buffer-definitions-async (title)
  "Update definitions from current buffer via MCP asynchronously.
Displays result with TITLE when complete."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (ctx (unison-ts-mcp--get-project-context))
        (position (point)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (let ((project-name (alist-get 'projectName ctx))
          (branch-name (alist-get 'branchName ctx)))
      (unison-ts-mcp--call-tool
       "update-definitions"
       (append (unison-ts-mcp--make-project-context project-name branch-name)
               `((code . ((text . ,code)))))
       (lambda (result)
         (unison-ts--display-mcp-result result title position))))))

;;;###autoload
(defun unison-ts-add ()
  "Add definitions from the current file to the codebase via MCP."
  (interactive)
  (unison-ts--update-buffer-definitions-async "add"))

;;;###autoload
(defun unison-ts-update ()
  "Update existing definitions in the codebase via MCP."
  (interactive)
  (unison-ts--update-buffer-definitions-async "update"))

;;;###autoload
(defun unison-ts-test ()
  "Run tests in the current project via MCP."
  (interactive)
  (let ((ctx (unison-ts-mcp--get-project-context))
        (position (point)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (unison-ts-mcp--call-tool
     "run-tests"
     (unison-ts-mcp--make-project-context
      (alist-get 'projectName ctx)
      (alist-get 'branchName ctx))
     (lambda (result)
       (unison-ts--display-mcp-result result "test" position)))))

;;;###autoload
(defun unison-ts-eval (expr)
  "Evaluate a Unison expression via MCP.
EXPR is evaluated using the typecheck-code tool with > prefix.
Works for both pure functions and IO actions."
  (interactive "sExpression: ")
  (let ((ctx (unison-ts-mcp--get-project-context))
        (position (point)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (unison-ts-mcp--call-tool
     "typecheck-code"
     (append (unison-ts-mcp--make-project-context
              (alist-get 'projectName ctx)
              (alist-get 'branchName ctx))
             `((code . ((sourceCode . ,(concat "> " expr))))))
     (lambda (result)
       (unison-ts--display-mcp-result result "eval" position)))))

;;;###autoload
(defun unison-ts-run ()
  "Run an IO action from the codebase via MCP.
For pure expressions, use `unison-ts-eval' instead."
  (interactive)
  (let ((term (read-string "IO action to run: "))
        (ctx (unison-ts-mcp--get-project-context))
        (position (point)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (unison-ts-mcp--call-tool
     "run"
     (append (unison-ts-mcp--make-project-context
              (alist-get 'projectName ctx)
              (alist-get 'branchName ctx))
             `((mainFunctionName . ,term)
               (args . [])))
     (lambda (result)
       (unison-ts--display-mcp-result result "run" position)))))

;;;###autoload
(defun unison-ts-watch ()
  "Typecheck current file and show results via MCP."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (ctx (unison-ts-mcp--get-project-context))
        (position (point)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (unison-ts-mcp--call-tool
     "typecheck-code"
     (append (unison-ts-mcp--make-project-context
              (alist-get 'projectName ctx)
              (alist-get 'branchName ctx))
             `((code . ((sourceCode . ,code)))))
     (lambda (result)
       (unison-ts--display-mcp-result result "watch" position)))))

;;;###autoload
(defun unison-ts-load ()
  "Load current file into the codebase via MCP."
  (interactive)
  (unison-ts--update-buffer-definitions-async "load"))

;;;###autoload
(defun unison-ts-send-region (start end)
  "Send the region between START and END to UCM via MCP."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region active"))
  (let* ((code (buffer-substring-no-properties start end))
         (result (unison-ts-mcp--update-definitions code)))
    (unison-ts--display-mcp-result result "eval" end)))

(defun unison-ts--definition-node-p (node)
  "Return non-nil if NODE is a Unison definition."
  (member (treesit-node-type node)
          '("term_declaration" "type_declaration" "ability_declaration")))

(defun unison-ts--definition-at-point ()
  "Return (CODE . END) for the definition at point.
CODE is the source text of the enclosing definition node.  END is
the buffer position after the node, suitable for anchoring result
overlays.  Signals `user-error' if point is not on a definition."
  (let ((node (treesit-node-at (point))))
    (unless node
      (user-error "No tree-sitter node at point"))
    (let ((def-node (treesit-parent-until node #'unison-ts--definition-node-p t)))
      (unless def-node
        (user-error "Point is not within a definition"))
      (cons (treesit-node-text def-node t)
            (treesit-node-end def-node)))))

;;;###autoload
(defun unison-ts-send-definition ()
  "Send the definition at point to UCM via MCP."
  (interactive)
  (let* ((info (unison-ts--definition-at-point))
         (code (car info))
         (end (cdr info))
         (result (unison-ts-mcp--update-definitions code)))
    (unison-ts--display-mcp-result result "eval" end)))

;;;###autoload
(defun unison-ts-eval-and-go (expr)
  "Evaluate EXPR via the UCM MCP REPL and switch to the REPL buffer.
Inserts \"> EXPR\" as a typed-prompt entry and switches to the REPL,
mirroring `unison-ts-eval' but routing output through the REPL buffer
instead of the minibuffer or a separate output buffer."
  (interactive "sExpression: ")
  (unison-ts--send-to-repl-and-go (concat "> " expr)))

;;;###autoload
(defun unison-ts-send-region-and-go (start end)
  "Send the region between START and END to the UCM MCP REPL and switch to it.
Inserts \"add <region>\" as a typed-prompt entry and switches to the
REPL buffer, mirroring `unison-ts-send-region'."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region active"))
  (let ((code (buffer-substring-no-properties start end)))
    (unison-ts--send-to-repl-and-go (concat "add " code))))

;;;###autoload
(defun unison-ts-send-definition-and-go ()
  "Send the definition at point to the UCM MCP REPL and switch to it.
Inserts \"add <definition>\" as a typed-prompt entry and switches to
the REPL buffer, mirroring `unison-ts-send-definition'."
  (interactive)
  (unison-ts--send-to-repl-and-go
   (concat "add " (car (unison-ts--definition-at-point)))))

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
