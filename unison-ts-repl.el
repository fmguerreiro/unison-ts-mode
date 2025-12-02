;;; unison-ts-repl.el --- UCM integration for unison-ts-mode -*- lexical-binding: t; -*-

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
;; UCM (Unison Codebase Manager) integration for unison-ts-mode.
;; Provides REPL interaction and command execution.
;;
;;; Code:

(require 'comint)
(require 'compile)
(require 'project)

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
