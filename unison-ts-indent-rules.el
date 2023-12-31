;;; unison-ts-indent-rules.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Filipe Guerreiro
;;
;; Author: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Maintainer: Filipe Guerreiro <filipe.m.guerreiro@gmail.com>
;; Created: November 23, 2023
;; Modified: November 23, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fmguerreiro/unison-ts-indent-rules
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defconst unison-ts-indent-rules--capture-parent-bol-pattern
  (regexp-opt '("term_definition" "type_declaration" "nested" "pattern"
                "tuple_or_parenthesized" "literal_list" "tuple_pattern" "function_application"
                "exp_if" "constructor" "delay_block" "type_signature")))

(defconst unison-ts-indent-rules--capture-node-eol-pattern
  (regexp-opt '("kw_then" "kw_else")))

;; docs:
;; jump-to-definition -> treesit-simple-indent-presets
;; jump-to-definition -> treesit-simple-indent-rules

(setq-default tab-width 2)
;; where, let, do, of, use, cases, nested, else ?
(defvar unison-ts-indent-rules)
(setq unison-ts-indent-rules
      `((unison
         ((node-is ,unison-ts-indent-rules--capture-node-eol-pattern) parent 0)
         ((parent-is ,unison-ts-indent-rules--capture-parent-bol-pattern) parent-bol ,tab-width)
         ((parent-is "ERROR") prev-line ,tab-width)
         (no-node parent 0))))

(provide 'unison-ts-indent-rules)
;;; unison-ts-indent-rules.el ends here
