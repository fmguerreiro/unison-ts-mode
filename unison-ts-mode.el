;;; unison-ts-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Filipe Guerreiro
;;
;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Modified: November 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fmguerreiro/unison-ts-mode
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A tree-sitter-based major mode for Unison.
;;
;;  Forked from https://github.com/dariooddenino/unison-ts-mode-emacs.
;;
;;; Code:

(require 'unison-ts-syntax-table)
(require 'unison-ts-setup)
(require 'unison-ts-install)

;;;###autoload
(define-derived-mode unison-ts-mode prog-mode "Unison"
  "Major mode for editing Unison, powered by tree-sitter."
  :group 'unison-ts
  :syntax-table unison-ts-syntax-table

  (when (and (unison-ts-ensure-grammar)
             (treesit-ready-p 'unison))
    (treesit-parser-create 'unison)
    (unison-ts-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.u\\'" . unison-ts-mode))
(add-to-list 'auto-mode-alist '("\\.unison\\'" . unison-ts-mode))
;; for tree-sitter-debug-mode
;; (add-to-list 'tree-sitter-major-mode-language-alist '(unison-ts-mode . unison))

;;; LSP Integration

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
    :new-connection (lsp-tcp-connection (lambda (_) '("localhost" 5757)))
    :activation-fn (lsp-activate-on "unison")
    :server-id 'unison-lsp
    :major-modes '(unison-ts-mode)
    :priority -1)))

(provide 'unison-ts-mode)
;;; unison-ts-mode.el ends here
