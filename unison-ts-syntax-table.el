;;; unison-ts-syntax-table.el --- Syntax table for unison-ts-mode -*- lexical-binding: t; -*-

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

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-syntax-table.el ends here
