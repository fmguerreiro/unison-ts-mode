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
(require 'unison-ts-font-lock)
(require 'unison-ts-indent-rules)

(defun unison-ts-setup ()
  "Setup treesit for unison-ts-mode."
  ;; (interactive) ;; TODO: remove

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     unison-ts-font-lock))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              ;; @ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Levels-of-Font-Lock.html
              '((comment doc string declaration preprocessor error import)
                (keyword type constant)
                (function-call variable)
                (bracket operator delimiter)))
  (setq-local treesit-font-lock-level 4)

  ;; TODO: remove: (setq-local treesit--indent-verbose t)
  (setq-local treesit-simple-indent-rules unison-ts-indent-rules)

  (treesit-major-mode-setup))

(provide 'unison-ts-setup)
;;; unison-ts-setup.el ends here
