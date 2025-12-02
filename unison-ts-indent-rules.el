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

(defvar unison-ts-indent-offset 2
  "Number of spaces for each indentation step in `unison-ts-mode'.")

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
Uses PARENT to find the containing term_definition and compute indent.
Returns BOL if no term_definition is found."
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

(defvar unison-ts-indent-rules
  `((unison
     ((parent-is "unison") column-0 0)

     ((and (node-is "kw_let") (parent-is "exp_let"))
      unison-ts--let-binding-anchor ,unison-ts-indent-offset)

     ((and (parent-is "exp_let") (not (node-is "kw_let")))
      unison-ts--let-binding-anchor ,(* 2 unison-ts-indent-offset))

     ((and (parent-is "term_definition") (node-is "pattern"))
      parent-bol ,unison-ts-indent-offset)

     ((parent-is "type_declaration") parent-bol ,unison-ts-indent-offset)

     ((parent-is "ability_declaration") parent-bol ,unison-ts-indent-offset)

     ((node-is "kw_then") parent 0)
     ((node-is "kw_else") parent 0)

     ((parent-is "exp_if") parent-bol ,unison-ts-indent-offset)

     (unison-ts--inside-let-binding-p
      unison-ts--let-binding-anchor-v2 ,(* 2 unison-ts-indent-offset))

     ((parent-is "tuple_or_parenthesized") parent-bol ,unison-ts-indent-offset)
     ((parent-is "literal_list") parent-bol ,unison-ts-indent-offset)
     ((parent-is "delay_block") parent-bol ,unison-ts-indent-offset)
     ((parent-is "nested") parent-bol ,unison-ts-indent-offset)
     ((parent-is "tuple_pattern") parent-bol ,unison-ts-indent-offset)
     ((parent-is "function_application") parent-bol ,unison-ts-indent-offset)
     ((parent-is "constructor") parent-bol ,unison-ts-indent-offset)
     ((parent-is "type_signature") parent-bol ,unison-ts-indent-offset)

     ((parent-is "ERROR") prev-line ,unison-ts-indent-offset)

     (no-node parent 0)))
  "Tree-sitter indentation rules for `unison-ts-mode'.")

(provide 'unison-ts-indent-rules)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-indent-rules.el ends here
