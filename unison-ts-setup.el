;;; ts-setup.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Filipe Guerreiro
;;
;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 11, 2023
;; Modified: November 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fmguerreiro/ts-setup
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

(defvar unison-ts-font-lock-rules
  '(;; Comments
    :language unison
    :override t
    :feature comment
    ((comment) @font-lock-comment-face)

    :language unison
    :feature identifier
    ((identifier (wordy_id) @font-lock-variable-name-face))))

(defun ts-setup ()
  "Setup treesit for unison-ts-mode."
  ;; Our tree-sitter setup goes here.

  ;; This handles font locking -- more on that below.
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     unison-ts-font-lock-rules))

  ;; This handles indentation -- again, more on that below.
  ;; (setq-local treesit-simple-indent-rules unison-ts-indent-rules)

  ;; ... everything else we talk about go here also ...

  ;; End with this
  (treesit-major-mode-setup))

(provide 'ts-setup)
;;; ts-setup.el ends here
