;;; unison-ts-setup.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Filipe Guerreiro
;;
;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Modified: November 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fmguerreiro/unison-ts-setup
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'treesit)

;; possible faces
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Faces.html
(defvar unison-ts-setup-font-lock-rules
  '(:feature comment
    :override t
    :language unison
    ((comment) @font-lock-comment-face)

    :feature doc
    :language unison
    ((doc_block) @font-lock-doc-face)

    :feature string
    :language unison
    ([(literal_char) (literal_text)] @font-lock-string-face)

    :feature constant
    :language unison
    ([(nat) (int) (float) (literal_boolean) (literal_byte)
      (literal_list) ;; TODO: maybe not!, it might overlap with other things
      (hash_qualifier)]
     @font-lock-constant-face)

    :feature variable
    :language unison
    ((wordy_id) @font-lock-variable-name-face)

    :feature declaration
    :override t
    :language unison
    ([(ability_declaration (wordy_id) @font-lock-function-name-face)
      (term_definition (wordy_id) @font-lock-function-name-face)
      (type_signature (wordy_id) @font-lock-function-name-face)
      (term_definition :anchor (wordy_id) @font-lock-function-name-face)])

    :feature type
    :language unison
    ([(type_name) @font-lock-type-face
      (path) @font-lock-type-face
      (namespace) @font-lock-type-face
      (term_type (wordy_id)) @font-lock-type-face
      ;; constructors
      (ability :anchor (wordy_id) @font-lock-type-face)
      (effect :anchor (wordy_id) @font-lock-type-face)])

    ;; @ref https://www.unison-lang.org/learn/language-reference/identifiers/#reserved-words
    :feature keyword
    :language unison
    ([(use) (structural_kw) (cases)
      (ability) (unique) (where) (operator)
      (type_kw) (kw_if) (kw_then) (kw_else)
      (tuple_or_parenthesized) ;; TODO: maybe not!, it might overlap with other things
      (kw_equals) (type_signature_colon) (arrow_symbol)
      (do) (delayed) (bang) (or) (and) (pipe)
      (forall) (handle) (alias) (let) (match) (with) (kw_typelink) (kw_termlink)] @font-lock-keyword-face)))

(defun unison-ts-setup ()
  "Setup treesit for unison-ts-mode."
  (interactive)

  ;; This handles font locking -- more on that below.
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     unison-ts-setup-font-lock-rules))

  (setq-local treesit-font-lock-level 5)

  ;; This handles indentation -- again, more on that below.
  ;; (setq-local treesit-simple-indent-rules unison-ts-indent-rules)

  ;; ... everything else we talk about go here also ...

  ;; End with this
  (treesit-major-mode-setup))

(provide 'unison-ts-setup)
;;; unison-ts-setup.el ends here
