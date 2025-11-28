;;; unison-ts-indent-rules.el --- Indentation rules for unison-ts-mode -*- lexical-binding: t; -*-

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
;; Defines tree-sitter indentation rules for Unison mode.
;;
;;; Code:

(setq-default tab-width 2)

(defun unison-ts--let-binding-anchor (_node parent bol &rest _)
  "Anchor for let bindings.
Uses PARENT to find the containing term_definition.
Computes indentation from containing term_definition, or returns BOL."
  (let* ((exp-let (unison-ts--find-exp-let-ancestor parent))
         (outer-term-def (when exp-let (treesit-node-parent exp-let))))
    (if outer-term-def
        (save-excursion
          (goto-char (treesit-node-start outer-term-def))
          (back-to-indentation)
          (point))
      bol)))

(defun unison-ts--find-exp-let-ancestor (node)
  "Find the exp_let ancestor of NODE, if any."
  (let ((current node))
    (while (and current (not (equal (treesit-node-type current) "exp_let")))
      (setq current (treesit-node-parent current)))
    current))

(defun unison-ts--inside-let-binding-p (node parent _bol &rest _)
  "Check if NODE is part of a term_declaration inside exp_let.
Uses PARENT to find the exp_let ancestor."
  (let ((exp-let (unison-ts--find-exp-let-ancestor parent)))
    (when exp-let
      (let ((term-decl-ancestor node))
        (while (and term-decl-ancestor
                    (not (equal (treesit-node-type term-decl-ancestor) "term_declaration"))
                    (not (eq term-decl-ancestor exp-let)))
          (setq term-decl-ancestor (treesit-node-parent term-decl-ancestor)))
        (and term-decl-ancestor
             (equal (treesit-node-type term-decl-ancestor) "term_declaration")
             (eq (treesit-node-parent term-decl-ancestor) exp-let))))))

(defun unison-ts--let-binding-anchor-v2 (_node parent bol &rest _)
  "Anchor for let bindings.
Uses PARENT to find the containing term_definition and compute indent, or returns BOL."
  (let ((exp-let (unison-ts--find-exp-let-ancestor parent)))
    (if exp-let
        (let ((outer-term-def (treesit-node-parent exp-let)))
          (if outer-term-def
              (save-excursion
                (goto-char (treesit-node-start outer-term-def))
                (back-to-indentation)
                (point))
            bol))
      bol)))

(defvar unison-ts-indent-rules)
(setq unison-ts-indent-rules
      `((unison
         ((parent-is "unison") column-0 0)

         ((and (node-is "kw_let") (parent-is "exp_let"))
          unison-ts--let-binding-anchor ,tab-width)

         ((and (parent-is "exp_let") (not (node-is "kw_let")))
          unison-ts--let-binding-anchor ,(* 2 tab-width))

         ((and (parent-is "term_definition") (node-is "pattern"))
          parent-bol ,tab-width)

         ((parent-is "type_declaration") parent-bol ,tab-width)

         ((parent-is "ability_declaration") parent-bol ,tab-width)

         ((node-is "kw_then") parent 0)
         ((node-is "kw_else") parent 0)

         ((parent-is "exp_if") parent-bol ,tab-width)

         (unison-ts--inside-let-binding-p unison-ts--let-binding-anchor-v2 ,(* 2 tab-width))

         ((parent-is "tuple_or_parenthesized") parent-bol ,tab-width)
         ((parent-is "literal_list") parent-bol ,tab-width)
         ((parent-is "delay_block") parent-bol ,tab-width)
         ((parent-is "nested") parent-bol ,tab-width)
         ((parent-is "tuple_pattern") parent-bol ,tab-width)
         ((parent-is "function_application") parent-bol ,tab-width)
         ((parent-is "constructor") parent-bol ,tab-width)
         ((parent-is "type_signature") parent-bol ,tab-width)

         ((parent-is "ERROR") prev-line ,tab-width)

         (no-node parent 0))))

(provide 'unison-ts-indent-rules)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-indent-rules.el ends here
