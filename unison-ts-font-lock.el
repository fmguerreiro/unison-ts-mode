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

(require 'treesit)

(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-validate "treesit.c")

;; @ref https://www.unison-lang.org/learn/language-reference/identifiers/#reserved-words
(defvar unison-ts-font-lock--keywords
  '("use" "!" "structural" "unique" "if" "then" "else"
    (ability) (namespace) (cases) (where) (do) (handle)
    (kw_let) (match) (with) (kw_typelink) (kw_termlink)
    (kw_forall) (type_kw)))

(defvar unison-ts-font-lock--constants
  '((nat) (int) (float) (literal_boolean) (literal_byte)
    (literal_hex) (hash_qualifier)))

(defvar unison-ts-font-lock-operators
  '((or) (and) (pipe) (operator) (kw_equals)
    (type_signature_colon) (arrow_symbol)))

(defvar unison-ts-font-lock)

;; possible faces
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;; syntax
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html
(setq unison-ts-font-lock
      `(:feature error
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

        :feature declaration
        :override t
        :language unison
        ([(constructor :anchor (wordy_id) @font-lock-function-name-face)
          ;; declarations with no args are highlighted as variable declarations
          (term_definition :anchor (path) :? (wordy_id) @font-lock-variable-name-face :anchor (kw_equals))
          ;; by default, declarations are highlighted as function declarations
          (term_definition :anchor (path) :? (wordy_id) @font-lock-function-name-face)
          (type_signature (wordy_id) @font-lock-function-name-face)])

        :feature keyword
        :language unison
        ([,@unison-ts-font-lock--keywords] @font-lock-keyword-face)

        :feature constant
        :override t
        :language unison
        ([,@unison-ts-font-lock--constants] @font-lock-constant-face)

        :feature variable
        :language unison
        ([((path) @font-lock-type-face :*)
          ((wordy_id) @font-lock-variable-use-face)
          ((type_argument) @font-lock-variable-use-face)
          (type_name (wordy_id) @font-lock-variable-name-face)])

        :feature preprocessor
        :language unison
        ((watch_expression) @font-lock-preprocessor-face)

        :feature type
        :override t
        :language unison
        ([((namespace) @font-lock-type-face)
          ((wordy_id) @font-lock-type-face (:match "^[A-Z][a-zA-Z_\\d]+" @font-lock-type-face))])

        :feature function-call
        :override t
        :language unison
        ([;; TODO arguments should be highlighted as variables as much as possible, but slow down the font-lock too much
          ;; ((wordy_id) @font-lock-variable-use-face :anchor (operator))
          ;; ((operator) :anchor (wordy_id) @font-lock-variable-use-face)
          ;; function name should be highlighted as a function
          (function_application :anchor (path) :? (wordy_id) @font-lock-function-call-face)])

        :feature bracket
        :language unison
        ([(tuple_or_parenthesized) (literal_list) (effect)] @font-lock-bracket-face)

        :feature operator
        :language unison
        ([,@unison-ts-font-lock-operators] @font-lock-operator-face)

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
