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

(defun unison-ts-mode-add-fold ()
  "Add a fold above the current line."
  (interactive)
  (newline)
  (newline)
  (newline)
  (save-excursion
    (forward-line -2)
    (insert "---")))

(defun unisonlang-delete-line ()
  "Delete the current line if empty."
  (let (start end content)
    (setq start (line-beginning-position))
    (setq end (line-end-position))
    (setq content (buffer-substring start end))
    (if (eq start end)
        (delete-region start (+ 1 end))
      (if (string-equal content "---")
          (delete-region start (+ 1 end))
        (forward-line 1)))))

(defun unison-ts-mode-remove-fold ()
  "Remove the fold directly above the current line."
  (interactive)
  (progn
    (goto-char (search-backward "---"))
    (forward-line -1)
    (unisonlang-delete-line)
    (unisonlang-delete-line)
    (unisonlang-delete-line)))

(defvar unison-ts-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-f") #'unison-ts-mode-add-fold)
    (define-key km (kbd "C-c C-d") #'unison-ts-mode-remove-fold)
    km)
  "Keymap for `unison-ts-mode'.")

;; (defun unisonlang-font-lock-extend-region ()
;;   "Extend the search region to include an entire block of text."
;;   ;; Avoid compiler warnings about these global variables from font-lock.el.
;;   ;; See the documentation for variable `font-lock-extend-region-functions'.
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (save-excursion
;;     (goto-char font-lock-beg)
;;     (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
;;       (goto-char font-lock-end)
;;       (when (re-search-forward "\n\n" nil t)
;;         (beginning-of-line)
;;         (setq font-lock-end (point)))
;;       (setq font-lock-beg found))))

;;;###autoload
(define-derived-mode unison-ts-mode prog-mode "unison-ts-mode"
  "Major mode for editing Unison."

  :syntax-table unison-ts-syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'unison)
    (treesit-parser-create 'unison)
    (unison-ts-setup))

  ;; definition, type, assignment, builtin, constant, keyword,
  ;; string-interpolation, comment, doc, string, operator, property,
  ;; preprocessor, escape-sequence, key (in key-value pairs)
  (setq-local treesit-font-lock-feature-list
              '((comment doc declaration preprocessor error)
                (constant keyword string type variable function-call)
                (bracket operator delimiter)))
  (setq font-lock-multiline t)
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.u\\'" . unison-ts-mode))

(after! tree-sitter
  (pushnew! tree-sitter-major-mode-language-alist
            '(unison-ts-mode . unison)))

(add-to-list 'tree-sitter-major-mode-language-alist '(unison-ts-mode . unison))

(provide 'unison-ts-mode)
;;; unison-ts-mode.el ends here
