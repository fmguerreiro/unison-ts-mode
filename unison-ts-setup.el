;;; unison-ts-setup.el --- Setup for unison-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Configures tree-sitter settings for Unison mode.
;;
;;; Code:

(require 'treesit)
(require 'unison-ts-font-lock)
(require 'unison-ts-indent-rules)

(defun unison-ts-setup ()
  "Setup treesit for unison-ts-mode."
  ;; (interactive) ;; TODO: remove

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     unison-ts-font-lock))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              ;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Levels-of-Font-Lock.html
              '((comment doc string declaration preprocessor error import)
                (keyword type constant)
                (function-call variable)
                (bracket operator delimiter)))
  (setq-local treesit-font-lock-level 4)

  ;; TODO: remove: (setq-local treesit--indent-verbose t)
  (setq-local treesit-simple-indent-rules unison-ts-indent-rules)

  (treesit-major-mode-setup))

(provide 'unison-ts-setup)
;;; unison-ts-setup.el ends here
