;;; unison-ts-mode-tests.el --- Tests for unison-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  ERT tests for unison-ts-mode.
;;  Requires the Unison tree-sitter grammar to be installed.
;;
;;; Code:

(require 'ert)
(require 'treesit)

(defvar unison-ts-mode-tests--grammar-available
  (treesit-ready-p 'unison t)
  "Non-nil if the Unison tree-sitter grammar is available.")

(defmacro unison-ts-mode-tests--with-buffer (content &rest body)
  "Create a temp buffer with CONTENT in `unison-ts-mode', then run BODY."
  (declare (indent 1))
  `(when unison-ts-mode-tests--grammar-available
     (require 'unison-ts-mode)
     (with-temp-buffer
       (insert ,content)
       (unison-ts-mode)
       (font-lock-ensure)
       ,@body)))

;;; Font-lock tests

(ert-deftest unison-ts-font-lock/keyword-highlighting ()
  "Keywords should be highlighted."
  (unison-ts-mode-tests--with-buffer "if true then 1 else 2"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/comment-highlighting ()
  "Comments should be highlighted."
  (unison-ts-mode-tests--with-buffer "-- this is a comment"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(ert-deftest unison-ts-font-lock/string-highlighting ()
  "Strings should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = \"hello\""
    (goto-char (point-min))
    (search-forward "\"hello\"")
    (backward-char 2)
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/function-definition ()
  "Function definitions should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo x y = x + y"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/type-highlighting ()
  "Types (capitalized identifiers) should be highlighted as types."
  (unison-ts-mode-tests--with-buffer "type MyType = Value"
    (goto-char (point-min))
    (search-forward "MyType")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/constant-nat ()
  "Natural numbers should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = 42"
    (goto-char (point-min))
    (search-forward "42")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

;;; Indentation tests

(ert-deftest unison-ts-indent/nested-expression ()
  "Nested expressions should be indented."
  (unison-ts-mode-tests--with-buffer "foo x =\n  x + 1"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/if-then-else ()
  "If-then-else should maintain proper indentation."
  (unison-ts-mode-tests--with-buffer "foo x =\n  if x\n  then 1\n  else 2"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 2))))

;;; Mode activation tests

(ert-deftest unison-ts-mode/activates-for-u-files ()
  "Mode should activate for .u files."
  (require 'unison-ts-mode)
  (should (eq (cdr (assoc "\\.u\\'" auto-mode-alist)) 'unison-ts-mode)))

(ert-deftest unison-ts-mode/activates-for-unison-files ()
  "Mode should activate for .unison files."
  (require 'unison-ts-mode)
  (should (eq (cdr (assoc "\\.unison\\'" auto-mode-alist)) 'unison-ts-mode)))

(provide 'unison-ts-mode-tests)
;;; unison-ts-mode-tests.el ends here
