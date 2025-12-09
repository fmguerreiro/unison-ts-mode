;;; unison-ts-font-lock.el --- Font-lock rules for unison-ts-mode -*- lexical-binding: t; -*-

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
;; Defines tree-sitter font-lock highlighting rules for Unison mode.
;;
;;; Code:

(require 'treesit)

(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-validate "treesit.c")

;; @ref https://www.unison-lang.org/learn/language-reference/identifiers/#reserved-words
(defvar unison-ts-font-lock--keywords
  '((use) (structural) (unique) (kw_if) (kw_then) (kw_else)
    (ability) (cases) (where) (do) (handle)
    (kw_let) (match) (with) (kw_typelink) (kw_termlink)
    (kw_forall) (type_kw) (otherwise)))

(defvar unison-ts-font-lock--constants
  '((nat) (int) (float) (literal_boolean) (literal_byte)
    (literal_hex) (hash_qualifier) (built_in_hash)))

(defvar unison-ts-font-lock-operators
  '((or) (and) (pipe) (operator) (kw_equals)
    (type_signature_colon) (arrow_symbol) (cons) (snoc)))

;; possible faces
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;; syntax
;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html
(defvar unison-ts-font-lock
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
        ([(constructor :anchor (regular_identifier) @font-lock-function-name-face)
          ;; Type constructors in ADT definitions (after = or |)
          (type_declaration (kw_equals) :anchor (regular_identifier) @font-lock-type-face)
          (type_declaration (pipe) :anchor (regular_identifier) @font-lock-type-face)
          ;; Ability operation names
          (ability_declaration (constructor (constructor_name) @font-lock-function-name-face))
          ;; Record field names
          (record_field (field_name) @font-lock-property-name-face)
          ;; declarations with no args are highlighted as variable declarations
          (term_definition name: (regular_identifier) @font-lock-variable-name-face :anchor (kw_equals))
          ;; by default, declarations are highlighted as function declarations
          (term_definition name: (regular_identifier) @font-lock-function-name-face)
          (type_signature term_name: (regular_identifier) @font-lock-function-name-face)])

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
          ((regular_identifier) @font-lock-variable-use-face)
          ((type_argument) @font-lock-variable-use-face)
          (type_name (regular_identifier) @font-lock-variable-name-face)])

        :feature preprocessor
        :language unison
        ((watch_expression) @font-lock-preprocessor-face)

        :feature import
        :override t
        :language unison
        ([(use_clause (namespace (regular_identifier) @font-lock-type-face))
          (use_clause (namespace (path) @font-lock-type-face))
          (use_clause (regular_identifier) @font-lock-type-face)])

        :feature type
        :override t
        :language unison
        ([((namespace) @font-lock-type-face)
          ((regular_identifier) @font-lock-type-face (:match "^[A-Z][a-zA-Z_\\d]+" @font-lock-type-face))])

        ;; TODO: function_application node no longer exists in grammar
        ;; :feature function-call
        ;; :override t
        ;; :language unison
        ;; ([;; function name should be highlighted as a function
        ;;   (function_application :anchor (path) :? (regular_identifier) @font-lock-function-call-face)])

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
        ([(literal_char) (literal_text)] @font-lock-string-face))
  "Tree-sitter font-lock settings for `unison-ts-mode'.")

(provide 'unison-ts-font-lock)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-font-lock.el ends here
