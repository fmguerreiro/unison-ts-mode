;;; unison-ts-inferior-mode.el --- Run the full-fledged UCM as a inferior process -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Filipe Guerreiro
;; Copyright (C) 2026 Maciej Barć

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run the full-fledged UCM (Unison Codebase Manager) as a inferior process
;; right inside GNU Emacs.

;;; Code:

(require 'comint)

(defgroup unison-ts-inferior-mode nil
  "UCM integration for Unison."
  :group 'unison-ts
  :prefix "unison-ts-inferior-mode-")

(defcustom ucm-executable (executable-find "ucm")
  "Path to the UCM executable."
  :type 'file
  :safe 'stringp
  :group 'unison-ts-inferior-mode)

(defcustom ucm-flags '()
  "Flags to start UCM with."
  :type '(repeat string)
  :group 'unison-ts-inferior-mode)

(defun unison-ts-inferior--ensure-ucm ()
  "Ensure UCM executable is available.
Signals an error if UCM is not found."
  (unless ucm-executable
    (user-error "UCM not found. Please, install it from https://unison-lang.org")))

(defun unison-ts-inferior--make-comint (buffer)
  "Run inferior UCM in BUFFER."
  (apply #'make-comint-in-buffer
         "Unison Codebase Manager"
         buffer
         ucm-executable
         nil
         ucm-flags))

(defun unison-ts-inferior-run-ucm ()
  (unison-ts-inferior--ensure-ucm)
  (let* ((buf-name "*Unison Codebase Manager*")
         (buffer (get-buffer-create buf-name))
         (process (comint-check-proc buf-name)))
    (unless process
      (with-current-buffer buffer
        (unison-ts-inferior--make-comint buffer)))
    buffer))

;;;###autoload
(defun inferior-ucm ()
  "Run the full-fledged UCM (Unison Codebase Manager) as a inferior process."
  (interactive)
  (let ((buffer (unison-ts-inferior-run-ucm)))
    (switch-to-buffer buffer)))

(provide 'unison-ts-inferior-mode)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-inferior-mode.el ends here
