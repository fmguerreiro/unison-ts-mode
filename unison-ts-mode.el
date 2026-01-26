;;; unison-ts-mode.el --- Tree-sitter support for Unison -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages unison tree-sitter
;; URL: https://github.com/fmguerreiro/unison-ts-mode

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

;; This package provides tree-sitter powered major mode for the Unison
;; programming language (https://www.unison-lang.org/).
;;
;; Features:
;; - Syntax highlighting with tree-sitter
;; - Automatic indentation
;; - imenu support for navigation
;; - LSP integration (eglot and lsp-mode)
;; - Auto-install of tree-sitter grammar
;; - UCM (Unison Codebase Manager) integration with REPL and commands
;;
;; Usage:
;; The mode is automatically activated for .u and .unison files.
;; For LSP support, install UCM (Unison Codebase Manager) and enable
;; eglot or lsp-mode.
;;
;; UCM Keybindings (under C-c C-u prefix):
;;   C-c C-u r - Open UCM REPL
;;   C-c C-u a - Add definitions from current file
;;   C-c C-u u - Update definitions
;;   C-c C-u t - Run tests
;;   C-c C-u x - Run a term
;;   C-c C-u w - Watch current file
;;   C-c C-u l - Load current file
;;   C-c C-u e - Send region to REPL
;;   C-c C-u d - Send definition at point to REPL
;;
;; Forked from https://github.com/dariooddenino/unison-ts-mode-emacs.

;;; Code:

(require 'treesit)
(require 'unison-ts-font-lock)
(require 'unison-ts-indent-rules)
(require 'unison-ts-install)
(require 'unison-ts-repl)

;;; Syntax Table

(defconst unison-ts-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\[ ". 1" table)
    (modify-syntax-entry ?: ". 23b" table)
    (modify-syntax-entry ?\] ". 4" table)
    table)
  "Syntax table for `unison-ts-mode'.")

;;; Setup

(defun unison-ts-setup ()
  "Setup treesit for `unison-ts-mode'."
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     unison-ts-font-lock))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '((comment doc string declaration preprocessor error import)
                (keyword type constant)
                (function-call variable)
                (bracket operator delimiter)))
  (setq-local treesit-font-lock-level 4)

  (setq-local treesit-simple-indent-rules unison-ts-indent-rules)

  (treesit-major-mode-setup))

;;; Imenu

(defun unison-ts-mode--defun-name (node)
  "Return the name of NODE for imenu."
  (pcase (treesit-node-type node)
    ("term_declaration"
     (when-let* ((term-def (treesit-search-subtree node "term_definition" nil nil 1))
                 (name-node (treesit-node-child term-def 0)))
       (treesit-node-text name-node t)))
    ("type_declaration"
     (when-let* ((type-constructor (treesit-node-child node 1))
                 (type-name (treesit-node-child type-constructor 0)))
       (treesit-node-text type-name t)))
    ("ability_declaration"
     (when-let* ((ability-name (treesit-search-subtree node "ability_name" nil nil 1))
                 (name-identifier (treesit-node-child ability-name 0)))
       (treesit-node-text name-identifier t)))
    (_ nil)))

(defvar unison-ts-mode-imenu-settings
  `(("Functions" "\\`term_declaration\\'" nil ,#'unison-ts-mode--defun-name)
    ("Types" "\\`type_declaration\\'" nil ,#'unison-ts-mode--defun-name)
    ("Abilities" "\\`ability_declaration\\'" nil ,#'unison-ts-mode--defun-name))
  "Imenu settings for Unison.
Each entry is (CATEGORY REGEXP PRED NAME-FN).
See `treesit-simple-imenu-settings' for details.")

(defvar unison-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-u r") #'unison-ts-repl)
    (define-key map (kbd "C-c C-u a") #'unison-ts-add)
    (define-key map (kbd "C-c C-u u") #'unison-ts-update)
    (define-key map (kbd "C-c C-u t") #'unison-ts-test)
    (define-key map (kbd "C-c C-u x") #'unison-ts-run)
    (define-key map (kbd "C-c C-u v") #'unison-ts-eval)
    (define-key map (kbd "C-c C-u w") #'unison-ts-watch)
    (define-key map (kbd "C-c C-u l") #'unison-ts-load)
    (define-key map (kbd "C-c C-u e") #'unison-ts-send-region)
    (define-key map (kbd "C-c C-u d") #'unison-ts-send-definition)
    map)
  "Keymap for `unison-ts-mode'.")

(easy-menu-define unison-ts-mode-menu unison-ts-mode-map
  "Menu for Unison mode."
  '("Unison"
    ["Open UCM REPL" unison-ts-repl]
    "---"
    ("UCM Commands"
     ["Add" unison-ts-add :active buffer-file-name]
     ["Update" unison-ts-update]
     ["Load" unison-ts-load :active buffer-file-name]
     ["Watch" unison-ts-watch :active buffer-file-name]
     "---"
     ["Send Region" unison-ts-send-region :active (use-region-p)]
     ["Send Definition" unison-ts-send-definition]
     "---"
     ["Test..." unison-ts-test]
     ["Run..." unison-ts-run])))

;;;###autoload
(define-derived-mode unison-ts-mode prog-mode "Unison"
  "Major mode for editing Unison, powered by tree-sitter."
  :group 'unison-ts
  :syntax-table unison-ts-syntax-table
  :keymap unison-ts-mode-map

  (when (and (unison-ts-ensure-grammar)
             (treesit-ready-p 'unison))
    (treesit-parser-create 'unison)
    (unison-ts-setup)

    (setq-local treesit-simple-imenu-settings unison-ts-mode-imenu-settings)
    (setq-local imenu-create-index-function #'treesit-simple-imenu)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.u\\'" . unison-ts-mode))
(add-to-list 'auto-mode-alist '("\\.unison\\'" . unison-ts-mode))
;; for tree-sitter-debug-mode
;; (add-to-list 'tree-sitter-major-mode-language-alist '(unison-ts-mode . unison))

;;; LSP Integration

(defvar eglot-server-programs)
(defvar lsp-language-id-configuration)

(declare-function lsp-register-client "ext:lsp-mode")
(declare-function make-lsp-client "ext:lsp-mode")
(declare-function lsp-tcp-connection "ext:lsp-mode")
(declare-function lsp-activate-on "ext:lsp-mode")

(defun unison-ts-mode--eglot-contact (_interactive)
  "Contact function for eglot to connect to UCM LSP server.
Starts UCM in headless mode if not already running."
  (let ((port (or (getenv "UNISON_LSP_PORT") "5757")))
    (unless (ignore-errors
              (delete-process
               (make-network-process
                :name "unison-lsp-check"
                :host "127.0.0.1"
                :service (string-to-number port)
                :nowait nil)))
      (start-process "ucm-headless" nil "ucm" "headless")
      (sleep-for 1))
    (list "127.0.0.1" (string-to-number port))))

(defun unison-ts-mode--kill-ucm-lsp (&rest _)
  "Kill any UCM process listening on the LSP port.
Called after eglot shuts down to clean up orphaned UCM processes."
  (let ((port (string-to-number (or (getenv "UNISON_LSP_PORT") "5757"))))
    (when-let ((output (shell-command-to-string
                        (format "lsof -t -i :%d 2>/dev/null" port))))
      (dolist (pid (split-string output "\n" t))
        (when (string-match-p "^[0-9]+$" pid)
          (ignore-errors
            (signal-process (string-to-number pid) 'TERM)))))))

(declare-function eglot-shutdown "ext:eglot")

;;;###autoload
(defun unison-ts-mode-setup-eglot ()
  "Set up eglot for `unison-ts-mode'.
Call this from your init file:
  (with-eval-after-load \\='unison-ts-mode
    (with-eval-after-load \\='eglot
      (unison-ts-mode-setup-eglot)))"
  (add-to-list 'eglot-server-programs
               '(unison-ts-mode . unison-ts-mode--eglot-contact))
  ;; Clean up UCM process after eglot shuts down (or times out)
  (advice-add 'eglot-shutdown :after #'unison-ts-mode--kill-ucm-lsp))

;;;###autoload
(defun unison-ts-mode-setup-lsp ()
  "Set up lsp-mode for `unison-ts-mode'.
Call this from your init file:
  (with-eval-after-load \\='unison-ts-mode
    (with-eval-after-load \\='lsp-mode
      (unison-ts-mode-setup-lsp)))"
  (add-to-list 'lsp-language-id-configuration '(unison-ts-mode . "unison"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection
                     (lambda (_port)
                       (let ((lsp-port (or (getenv "UNISON_LSP_PORT") "5757")))
                         (unless (ignore-errors
                                   (delete-process
                                    (make-network-process
                                     :name "unison-lsp-check-lsp-mode"
                                     :host "127.0.0.1"
                                     :service (string-to-number lsp-port)
                                     :nowait nil)))
                           (start-process "ucm-headless-lsp-mode" nil "ucm" "headless")
                           (sleep-for 1))
                         (cons "localhost" (string-to-number lsp-port)))))
    :activation-fn (lsp-activate-on "unison")
    :server-id 'unison-lsp
    :major-modes '(unison-ts-mode)
    :priority -1)))

(provide 'unison-ts-mode)
;;; unison-ts-mode.el ends here
