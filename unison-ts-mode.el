;;; unison-ts-mode.el --- Tree-sitter support for Unison -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Version: 0.1.0
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
;;
;; Usage:
;; The mode is automatically activated for .u and .unison files.
;; For LSP support, install UCM (Unison Codebase Manager) and enable
;; eglot or lsp-mode.
;;
;; Forked from https://github.com/dariooddenino/unison-ts-mode-emacs.

;;; Code:

(require 'unison-ts-syntax-table)
(require 'unison-ts-setup)
(require 'unison-ts-install)

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

;;;###autoload
(define-derived-mode unison-ts-mode prog-mode "Unison"
  "Major mode for editing Unison, powered by tree-sitter."
  :group 'unison-ts
  :syntax-table unison-ts-syntax-table

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

;; Optional LSP integration using `with-eval-after-load' to avoid
;; requiring eglot/lsp-mode as dependencies.  This is the standard
;; pattern for optional integrations in tree-sitter modes.

(defun unison-ts-mode--eglot-contact (_interactive)
  "Contact function for eglot to connect to UCM LSP server.
Starts UCM in headless mode if not already running."
  (let ((port (or (getenv "UNISON_LSP_PORT") "5757")))
    ;; Start UCM headless in background if not already running
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

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(unison-ts-mode . unison-ts-mode--eglot-contact)))

;;;###autoload
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(unison-ts-mode . "unison"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection
                     (lambda (_port)
                       (let ((lsp-port (or (getenv "UNISON_LSP_PORT") "5757")))
                         ;; Start UCM headless if not already running
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
