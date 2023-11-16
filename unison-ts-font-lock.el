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
(setq unison-ts-font-lock
      (let* (
             ;; Regex for identifiers

             ;; Type identifier
             (type-regexp "[A-Z_][A-Za-z_!'0-9]*")
             ;; A valid identifier
             ;; TODO include unicode characters
             (identifier-regexp "[A-Za-z_][A-Za-z_!'0-9]*")
             ;; namespaced identifier
             (namespaced-regexp (concat "\\(?:\\.\\|" identifier-regexp "\\)+"))

             ;; Handle the unison fold
             (x-fold-regexp "---\\(\n\\|.\\)*")

             ;; define several categories of keywords
             ;; symbol keywords
             (x-symbol-keywords '(":" "->"))
             ;; standard alphabetical keywords
             (x-keywords '("if" "then" "else" "forall" "handle" "unique" "where" "use" "and" "or" "true" "false" "type" "ability" "alias" "let" "namespace" "cases" "match" "with"))

             ;; generate regex strings for each keyword category
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-symbol-keywords-regexp (regexp-opt x-symbol-keywords 1))
             ;; (x-single-quote-exc-regexp (regexp-opt x-single-quote-exc 1))
             (x-keywords-full-regexp (concat x-keywords-regexp "\\|" x-symbol-keywords-regexp))

             (x-request-regexp "Request")

             ;; single quote or exclamation point when it's not part of an identifier
             (x-single-quote-exc-regexp "\\(\s\\)\\(!\\|'\\)")

             ;; Signautres
             (x-sig-regexp (concat "^\s*?\\(" namespaced-regexp "\\).+?[:=]"))

             ;; Namespaces definition
             (x-namespace-def-regexp (concat "namespace\s+\\(" namespaced-regexp "\\)\s+where"))
             ;; Namespaces import
             (x-namespace-import-regexp (concat "use\s+\\(" namespaced-regexp "\\)"))

             ;; Abilities
             (x-ability-def-regexp (concat "ability\s\\(" type-regexp "\\)\s.+"))
             ;;(x-ability-regexp (concat "{\\(?:.*\\|\\(" type-regexp "\\)\\)}"))
             (x-ability-regexp (concat "[{,].*?\\(" type-regexp "\\)"))

             (x-type-def-regexp (concat "type\s\\(" type-regexp "\\)\s.+"))
             (x-type-regexp (concat "[^a-z]\\(" type-regexp "\\)"))

             (x-esc-regexp "!"))


        `(
          (,x-fold-regexp . (0 font-lock-comment-face t))
          (,x-keywords-full-regexp . font-lock-keyword-face)
          (,x-single-quote-exc-regexp . (2 font-lock-keyword-face))
          (,x-request-regexp . font-lock-preprocessor-face)
          (,x-sig-regexp . (1 font-lock-function-name-face))
          (,x-namespace-def-regexp . (1 font-lock-constant-face))
          (,x-namespace-import-regexp . (1 font-lock-constant-face))
          (,x-ability-def-regexp . (1 font-lock-variable-name-face))
          (,x-ability-regexp . (1 font-lock-variable-name-face))
          (,x-type-def-regexp . (1 font-lock-type-face))
          (,x-type-regexp . (1 font-lock-type-face))
          (,x-esc-regexp . font-lock-negation-char-face))))

(provide 'unison-ts-font-lock)
;;; unison-ts-font-lock.el ends here
