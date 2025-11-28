;;; unison-ts-syntax-table.el --- Syntax table for unison-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Defines the Emacs syntax table for Unison mode.
;;
;;; Code:

(defconst unison-ts-syntax-table
  (let ((table (make-syntax-table)))
    ;; -- Are comments
    (modify-syntax-entry ?- ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    ;; [: :] for docs
    (modify-syntax-entry ?\[ ". 1" table)
    (modify-syntax-entry ?: ". 23b" table)
    (modify-syntax-entry ?\] ". 4" table)
    table))

(provide 'unison-ts-syntax-table)

;;; unison-ts-syntax-table.el ends here
