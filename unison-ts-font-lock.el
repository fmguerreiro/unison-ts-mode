;;; unison-ts-font-lock.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Filipe Guerreiro
;;
;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Modified: November 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fmguerreiro/font-lock
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar unison-ts-font-lock)

;; possible faces
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;; syntax
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html
(setq unison-ts-font-lock
      '(:feature error
        :override t
        :language unison
        ((ERROR) @font-lock-warning-face)

        :feature comment
        :override t
        :language unison
        ((comment) @font-lock-comment-face)

        :feature doc
        :override t
        :language unison
        ((doc_block) @font-lock-doc-face)

        ;; @ref https://www.unison-lang.org/learn/language-reference/identifiers/#reserved-words
        :feature keyword
        :language unison
        ([(use) (ability) (structural) (structural_kw) (cases) (unique) (where) (type_kw) (kw_if) (kw_then) (kw_else) "!" (do) (delayed) (match) (with) (kw_typelink) (kw_termlink) (forall) (handle)] @font-lock-keyword-face)
        ;; (alias) (let)

        :feature constant
        :override t
        :language unison
        ([(nat) (int) (float) (literal_boolean) (literal_byte) (literal_hex) (hash_qualifier)] @font-lock-constant-face)

        :feature variable
        :language unison
        ([((wordy_id) @font-lock-variable-name-face)
          ((path) :? @font-lock-type-face (wordy_id) @font-lock-variable-name-face)
          ((type_argument) @font-lock-variable-name-face)
          (type_name (wordy_id) @font-lock-variable-name-face)])

        :feature preprocessor
        :language unison
        ((watch_expression) @font-lock-preprocessor-face)

        :feature type
        :override t
        :language unison
        ([((ability) :anchor (wordy_id) @font-lock-type-face)
          (effect :anchor (wordy_id) @font-lock-type-face)
          ((path) @font-lock-type-face)
          ((namespace) @font-lock-type-face)
          ((type_signature_colon) (wordy_id) @font-lock-type-face)
          ((wordy_id) @font-lock-type-face (:match "^[A-Z][a-zA-Z_\\d]+" @font-lock-type-face))])

        :feature declaration
        :override t
        :language unison
        ([((path) :? @font-lock-type-face :anchor (wordy_id) @font-lock-function-name-face :anchor (type_signature_colon))
          (term_declaration (type_signature :anchor (path) :? @font-lock-type-face :anchor (wordy_id) @font-lock-function-name-face))
          (term_declaration (term_definition :anchor (path) :? @font-lock-type-face :anchor (wordy_id) @font-lock-function-name-face (kw_equals)))])

        :feature function-call
        :override t
        :language unison
        ([(function_application :anchor ((path) :? @font-lock-type-face :anchor (wordy_id) @font-lock-variable-name-face :anchor (operator)))
          (function_application :anchor ((path) :? @font-lock-type-face :anchor (wordy_id) @font-lock-function-call-face))])

        :feature bracket
        :language unison
        ([(tuple_or_parenthesized) (literal_list) (effect)] @font-lock-bracket-face)

        :feature operator
        :language unison
        ([(or) (and) (pipe) (operator) (kw_equals) (type_signature_colon) (arrow_symbol)] @font-lock-operator-face)

        ;; TODO: '.' as in List.map
        :feature delimiter
        :override t
        :language unison
        ([","] @font-lock-delimiter-face)

        :feature string
        :override t
        :language unison
        ([(literal_char) (literal_text)] @font-lock-string-face)))

(provide 'unison-ts-font-lock)
;;; unison-ts-font-lock.el ends here
