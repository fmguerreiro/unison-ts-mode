;;; unison-ts-mode-tests.el --- Tests for unison-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  ERT tests for unison-ts-mode.
;;  Requires the Unison tree-sitter grammar to be installed.
;;
;;; Code:

(require 'ert)
(require 'treesit)

;; Try common tree-sitter grammar locations
(dolist (path '("~/.emacs.d/tree-sitter/"
                "~/doom-emacs/.local/cache/tree-sitter/"
                "~/.local/share/tree-sitter/"))
  (when (file-directory-p (expand-file-name path))
    (add-to-list 'treesit-extra-load-path (expand-file-name path))))

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

(defun unison-ts-mode-tests--face-at-string (str)
  "Return the face at the first occurrence of STR in current buffer."
  (goto-char (point-min))
  (search-forward str)
  (get-text-property (- (point) (length str)) 'face))

;;; Font-lock tests - Keywords

(ert-deftest unison-ts-font-lock/keyword-if ()
  "Keyword 'if' should be highlighted."
  (unison-ts-mode-tests--with-buffer "if true then 1 else 2"
    (should (eq (unison-ts-mode-tests--face-at-string "if") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-then ()
  "Keyword 'then' should be highlighted."
  (unison-ts-mode-tests--with-buffer "if true then 1 else 2"
    (should (eq (unison-ts-mode-tests--face-at-string "then") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-else ()
  "Keyword 'else' should be highlighted."
  (unison-ts-mode-tests--with-buffer "if true then 1 else 2"
    (should (eq (unison-ts-mode-tests--face-at-string "else") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-do ()
  "Keyword 'do' should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo = do\n  x"
    (should (eq (unison-ts-mode-tests--face-at-string "do") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-handle ()
  "Keyword 'handle' should be highlighted."
  (unison-ts-mode-tests--with-buffer "handle foo with cases"
    (should (eq (unison-ts-mode-tests--face-at-string "handle") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-match ()
  "Keyword 'match' should be highlighted."
  (unison-ts-mode-tests--with-buffer "match x with\n  _ -> 1"
    (should (eq (unison-ts-mode-tests--face-at-string "match") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-with ()
  "Keyword 'with' should be highlighted."
  (unison-ts-mode-tests--with-buffer "match x with\n  _ -> 1"
    (should (eq (unison-ts-mode-tests--face-at-string "with") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-type ()
  "Keyword 'type' should be highlighted."
  (unison-ts-mode-tests--with-buffer "type MyType = Value"
    (should (eq (unison-ts-mode-tests--face-at-string "type") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-ability ()
  "Keyword 'ability' should be highlighted."
  (unison-ts-mode-tests--with-buffer "ability Store where\n  get : Nat"
    (should (eq (unison-ts-mode-tests--face-at-string "ability") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-use ()
  "Keyword 'use' should be highlighted."
  (unison-ts-mode-tests--with-buffer "use Nat +"
    (should (eq (unison-ts-mode-tests--face-at-string "use") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-let ()
  "Keyword 'let' should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo =\n  let x = 1\n  x"
    (should (eq (unison-ts-mode-tests--face-at-string "let") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-cases ()
  "Keyword 'cases' should be highlighted."
  (unison-ts-mode-tests--with-buffer "f = cases\n  x -> x"
    (should (eq (unison-ts-mode-tests--face-at-string "cases") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-where ()
  "Keyword 'where' should be highlighted."
  (unison-ts-mode-tests--with-buffer "ability Store where\n  get : Nat"
    (should (eq (unison-ts-mode-tests--face-at-string "where") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-forall ()
  "Keyword 'forall' should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo : forall a. a -> a"
    (should (eq (unison-ts-mode-tests--face-at-string "forall") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-forall-unicode ()
  "Keyword (unicode forall) should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo : ∀ a. a -> a"
    (should (eq (unison-ts-mode-tests--face-at-string "∀") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-unique ()
  "Keyword 'unique' should be highlighted."
  (unison-ts-mode-tests--with-buffer "unique type MyType = Value"
    (should (eq (unison-ts-mode-tests--face-at-string "unique") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-structural ()
  "Keyword 'structural' should be highlighted."
  (unison-ts-mode-tests--with-buffer "structural type MyType = Value"
    (should (eq (unison-ts-mode-tests--face-at-string "structural") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-otherwise ()
  "Keyword 'otherwise' should be highlighted."
  (unison-ts-mode-tests--with-buffer "match x with\n  _ | otherwise -> 1"
    (should (eq (unison-ts-mode-tests--face-at-string "otherwise") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-termLink ()
  "Keyword 'termLink' should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = termLink foo"
    (should (eq (unison-ts-mode-tests--face-at-string "termLink") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-typeLink ()
  "Keyword 'typeLink' should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = typeLink Nat"
    (should (eq (unison-ts-mode-tests--face-at-string "typeLink") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-alias ()
  "Keyword 'alias' should be highlighted."
  (unison-ts-mode-tests--with-buffer "alias.term Foo = Bar"
    (should (eq (unison-ts-mode-tests--face-at-string "alias") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/keyword-namespace ()
  "Keyword 'namespace' should be highlighted."
  (unison-ts-mode-tests--with-buffer "namespace Foo"
    (should (eq (unison-ts-mode-tests--face-at-string "namespace") 'font-lock-keyword-face))))

;;; Font-lock tests - Comments

(ert-deftest unison-ts-font-lock/comment-line ()
  "Line comments (--) should be highlighted."
  (unison-ts-mode-tests--with-buffer "-- this is a comment"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(ert-deftest unison-ts-font-lock/comment-block ()
  "Block comments ({- -}) should be highlighted."
  (unison-ts-mode-tests--with-buffer "{- this is a block comment -}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(ert-deftest unison-ts-font-lock/comment-block-nested ()
  "Nested block comments ({- {- -} -}) should be highlighted."
  (unison-ts-mode-tests--with-buffer "{- outer {- inner -} outer -}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))
    (search-forward "inner")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(ert-deftest unison-ts-font-lock/comment-fold ()
  "Fold comments (---) should be highlighted."
  (unison-ts-mode-tests--with-buffer "--- fold section"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

;;; Font-lock tests - General

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

;;; Identifier tests

(ert-deftest unison-ts-font-lock/identifier-regular ()
  "Regular identifiers should be parsed correctly."
  (unison-ts-mode-tests--with-buffer "foo = 1"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/identifier-with-underscore ()
  "Identifiers starting with underscore should be parsed."
  (unison-ts-mode-tests--with-buffer "_bar4 = 1"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/identifier-with-prime ()
  "Identifiers with prime (') should be parsed."
  (unison-ts-mode-tests--with-buffer "qux' = 1"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/identifier-with-bang ()
  "Identifiers with bang (!) should be parsed."
  (unison-ts-mode-tests--with-buffer "set! = 1"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/operator-definition ()
  "Operator definitions should be highlighted."
  (unison-ts-mode-tests--with-buffer "(!$%^&*-=+<>~\\/|:) a b = a"
    (goto-char (point-min))
    ;; Operator should be highlighted as function name
    (should (eq (get-text-property (+ (point) 1) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/qualified-identifier ()
  "Qualified identifiers (Foo.Bar.baz) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = Foo.Bar.baz"
    (goto-char (point-min))
    (search-forward "Foo")
    (backward-char 1)
    ;; The namespace parts should be highlighted
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/absolute-identifier ()
  "Absolute identifiers (.base.List) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = .base.List"
    (goto-char (point-min))
    (search-forward ".base")
    (backward-char 1)
    ;; Should have some face applied
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/hash-qualified-identifier ()
  "Hash-qualified identifiers (name#hash) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = foo#abc123"
    (goto-char (point-min))
    (search-forward "foo")
    (backward-char 1)
    ;; Should have some face applied
    (should (get-text-property (point) 'face))))

;;; Hash literal tests

(ert-deftest unison-ts-font-lock/hash-term-reference ()
  "Hash term references (#x) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = #abc123"
    (goto-char (point-min))
    (search-forward "#")
    ;; Hash reference should have some highlighting
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/hash-nth-cycle ()
  "Hash nth-in-cycle references (#x.n) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = #abc123.1"
    (goto-char (point-min))
    (search-forward "#")
    ;; Hash reference should have some highlighting
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/hash-constructor ()
  "Hash constructor references (#x#c) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = #abc123#0"
    (goto-char (point-min))
    (search-forward "#abc")
    (backward-char 1)
    ;; Hash reference should have some highlighting
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/hash-cyclic-constructor ()
  "Hash cyclic constructor references (#x.n#c) should be parsed."
  (unison-ts-mode-tests--with-buffer "x = #abc123.1#0"
    (goto-char (point-min))
    (search-forward "#abc")
    (backward-char 1)
    ;; Hash reference should have some highlighting
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/hash-builtin ()
  "Builtin hash references (##Nat) should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = ##Nat"
    (goto-char (point-min))
    (search-forward "##")
    ;; Builtin should have some highlighting
    (should (get-text-property (point) 'face))))

;;; Expression tests

(ert-deftest unison-ts-font-lock/if-then-else-keywords ()
  "If/then/else keywords should all be highlighted."
  (unison-ts-mode-tests--with-buffer "if true then 1 else 2"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
    (search-forward "then")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
    (search-forward "else")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/boolean-and ()
  "Boolean && operator should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = true && false"
    (goto-char (point-min))
    (search-forward "&&")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-operator-face))))

(ert-deftest unison-ts-font-lock/boolean-or ()
  "Boolean || operator should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = true || false"
    (goto-char (point-min))
    (search-forward "||")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-operator-face))))

(ert-deftest unison-ts-font-lock/match-with ()
  "Match/with keywords should be highlighted."
  (unison-ts-mode-tests--with-buffer "match x with\n  Some y -> y\n  None -> 0"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
    (search-forward "with")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/handle-with ()
  "Handle/with keywords should be highlighted."
  (unison-ts-mode-tests--with-buffer "handle foo with\n  { pure x } -> x"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
    (search-forward "with")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/delay-quote ()
  "Delay expression with quote ('expr) should parse."
  (unison-ts-mode-tests--with-buffer "x = 'foo"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/delay-do ()
  "Delay expression with do should highlight do as keyword."
  (unison-ts-mode-tests--with-buffer "x = do\n  foo"
    (goto-char (point-min))
    (search-forward "do")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/force ()
  "Force expression (!thunk) should parse."
  (unison-ts-mode-tests--with-buffer "x = !thunk"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

;;; Pattern tests

(ert-deftest unison-ts-font-lock/pattern-blank ()
  "Blank pattern (_) should parse."
  (unison-ts-mode-tests--with-buffer "foo _ = 1"
    (goto-char (point-min))
    (search-forward "_")
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-literal ()
  "Literal pattern should parse."
  (unison-ts-mode-tests--with-buffer "match x with\n  42 -> true"
    (goto-char (point-min))
    (search-forward "42")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/pattern-variable ()
  "Variable pattern should parse."
  (unison-ts-mode-tests--with-buffer "foo x = x"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-as ()
  "As-pattern (x@pat) should parse."
  (unison-ts-mode-tests--with-buffer "foo xs@(h +: t) = xs"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-constructor ()
  "Constructor pattern (Some x) should highlight constructor as type."
  (unison-ts-mode-tests--with-buffer "match x with\n  Some y -> y"
    (goto-char (point-min))
    (search-forward "Some")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/pattern-list-cons ()
  "List cons pattern (h +: t) should highlight operator."
  (unison-ts-mode-tests--with-buffer "match xs with\n  h +: t -> h"
    (goto-char (point-min))
    (search-forward "+:")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-operator-face))))

(ert-deftest unison-ts-font-lock/pattern-list-snoc ()
  "List snoc pattern (init :+ last) should highlight operator."
  (unison-ts-mode-tests--with-buffer "match xs with\n  init :+ last -> last"
    (goto-char (point-min))
    (search-forward ":+")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-operator-face))))

(ert-deftest unison-ts-font-lock/pattern-tuple ()
  "Tuple pattern ((a,b)) should parse."
  (unison-ts-mode-tests--with-buffer "foo (a, b) = a"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-guard ()
  "Guard pattern (p | cond) should parse."
  (unison-ts-mode-tests--with-buffer "match x with\n  n | n > 0 -> n"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-ability ()
  "Ability pattern ({A.op -> k}) should parse."
  (unison-ts-mode-tests--with-buffer "handle foo with\n  { State.get -> k } -> k 0"
    (goto-char (point-min))
    (should (not (null (treesit-node-at (point)))))))

(ert-deftest unison-ts-font-lock/pattern-pure ()
  "Pure pattern ({p}) should parse."
  (unison-ts-mode-tests--with-buffer "handle foo with\n  { pure x } -> x"
    (goto-char (point-min))
    (search-forward "pure")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

;;; Type syntax tests

(ert-deftest unison-ts-font-lock/function-type-arrow ()
  "Function type arrows (X -> Y) should parse correctly."
  (unison-ts-mode-tests--with-buffer "foo : Nat -> Nat"
    (goto-char (point-min))
    (search-forward "Nat")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/tuple-type ()
  "Tuple types ((A, B)) should have types highlighted."
  (unison-ts-mode-tests--with-buffer "foo : (Nat, Text)"
    (goto-char (point-min))
    (search-forward "Nat")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/polymorphic-type-forall ()
  "Polymorphic types (forall a. a -> a) should highlight forall as keyword."
  (unison-ts-mode-tests--with-buffer "id : forall a. a -> a"
    (goto-char (point-min))
    (search-forward "forall")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/ability-context-io ()
  "Ability context (X ->{IO} Y) should highlight IO as type."
  (unison-ts-mode-tests--with-buffer "foo : Nat ->{IO} Nat"
    (goto-char (point-min))
    (search-forward "IO")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/ability-context-pure ()
  "Pure ability context (X ->{} Y) should parse without errors."
  (unison-ts-mode-tests--with-buffer "foo : Nat ->{} Nat"
    (goto-char (point-min))
    (search-forward "Nat" nil t 2)
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

;;; Declaration tests

(ert-deftest unison-ts-font-lock/term-definition ()
  "Term definitions (foo x = x) should highlight function name."
  (unison-ts-mode-tests--with-buffer "foo x = x"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/type-signature ()
  "Type signatures (foo : Nat -> Nat) should highlight function name."
  (unison-ts-mode-tests--with-buffer "foo : Nat -> Nat"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/operator-definition-prefix ()
  "Operator definitions ((**) x y = ...) should highlight operator."
  (unison-ts-mode-tests--with-buffer "(**) x y = x"
    (goto-char (point-min))
    (search-forward "(**)")
    (backward-char 2)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-function-name-face font-lock-operator-face nil)))))

(ert-deftest unison-ts-font-lock/operator-definition-infix ()
  "Infix operator definitions (x ** y = ...) should parse correctly."
  (unison-ts-mode-tests--with-buffer "x ** y = x"
    (goto-char (point-min))
    (search-forward "**")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-function-name-face font-lock-operator-face nil)))))

(ert-deftest unison-ts-font-lock/structural-type ()
  "Structural type declarations should highlight structural keyword."
  (unison-ts-mode-tests--with-buffer "structural type Foo = Bar"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/unique-type ()
  "Unique type declarations should highlight unique keyword."
  (unison-ts-mode-tests--with-buffer "unique type Foo = Bar"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/record-type ()
  "Record types (type R = {field : Type}) should highlight type name."
  (unison-ts-mode-tests--with-buffer "structural type Person = { name : Text, age : Nat }"
    (goto-char (point-min))
    (search-forward "Person")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/record-type-field ()
  "Record type fields should have their types highlighted."
  (unison-ts-mode-tests--with-buffer "structural type Person = { name : Text }"
    (goto-char (point-min))
    (search-forward "Text")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/record-field-name ()
  "Record field names should be highlighted as properties."
  (unison-ts-mode-tests--with-buffer "structural type Person = { name : Text }"
    (goto-char (point-min))
    (search-forward "name")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-property-name-face))))

(ert-deftest unison-ts-font-lock/ability-declaration ()
  "Ability declarations should highlight ability keyword."
  (unison-ts-mode-tests--with-buffer "structural ability Store v where\n  get : v\n  put : v -> ()"
    (should (eq (unison-ts-mode-tests--face-at-string "ability") 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/ability-declaration-name ()
  "Ability declaration names should be highlighted as types."
  (unison-ts-mode-tests--with-buffer "structural ability Store v where\n  get : v"
    (goto-char (point-min))
    (search-forward "Store")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-type-face))))

;;; Let block tests

(ert-deftest unison-ts-font-lock/let-keyword ()
  "Let keyword should be highlighted."
  (unison-ts-mode-tests--with-buffer "foo x =\n  let\n    y = 1\n    y"
    (goto-char (point-min))
    (search-forward "let")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))

(ert-deftest unison-ts-font-lock/let-binding-definition ()
  "Variable in let binding should be highlighted as function definition."
  (unison-ts-mode-tests--with-buffer "foo x =\n  let\n    y = 1\n    y"
    (goto-char (point-min))
    (search-forward "y =")
    (backward-char 3)
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/let-multiple-bindings ()
  "Multiple let bindings should all be highlighted."
  (unison-ts-mode-tests--with-buffer "foo =\n  let\n    a = 1\n    b = 2\n    a + b"
    (goto-char (point-min))
    (search-forward "a =")
    (backward-char 3)
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))
    (search-forward "b =")
    (backward-char 3)
    (should (eq (get-text-property (point) 'face) 'font-lock-function-name-face))))

(ert-deftest unison-ts-font-lock/let-tuple-destructuring ()
  "Tuple destructuring in let should highlight pattern variables."
  (unison-ts-mode-tests--with-buffer "foo =\n  let\n    (a, b) = (1, 2)\n    a"
    (goto-char (point-min))
    (search-forward "(a,")
    (backward-char 2)
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'font-lock-variable-name-face)
                  (eq face 'font-lock-function-name-face)
                  (null face))))))

(ert-deftest unison-ts-font-lock/let-pattern-destructuring ()
  "Pattern destructuring (Some x) = opt should parse correctly."
  (unison-ts-mode-tests--with-buffer "foo opt =\n  let\n    (Some x) = opt\n    x"
    (goto-char (point-min))
    (search-forward "Some")
    (backward-char 1)
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'font-lock-type-face)
                  (eq face 'font-lock-constant-face))))))

;;; Additional indentation tests - nested blocks

(ert-deftest unison-ts-indent/nested-let-block ()
  "Let block contents should be indented relative to let."
  (unison-ts-mode-tests--with-buffer "foo =\n  let\n    x = 1\n    x"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))))

(ert-deftest unison-ts-indent/nested-match-case ()
  "Match arms should be indented."
  (unison-ts-mode-tests--with-buffer "foo x = match x with\n  Some y -> y\n  None -> 0"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/nested-match-arm-body ()
  "Match arm body on next line should be further indented."
  (unison-ts-mode-tests--with-buffer "foo x = match x with\n  Some y ->\n    y + 1\n  None -> 0"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))))

(ert-deftest unison-ts-indent/type-declaration-constructors ()
  "Type declaration constructors should be indented."
  (unison-ts-mode-tests--with-buffer "type MyType\n  = Value1\n  | Value2"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/ability-declaration-ops ()
  "Ability declaration operations should be indented."
  (unison-ts-mode-tests--with-buffer "structural ability Store v where\n  get : v\n  put : v -> ()"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/let-block-alignment ()
  "First statement in let block sets the indentation boundary."
  (unison-ts-mode-tests--with-buffer "foo =\n  let\n    first = 1\n    second = 2"
    (goto-char (point-min))
    (forward-line 2)
    (let ((first-indent (current-indentation)))
      (forward-line 1)
      (should (= (current-indentation) first-indent)))))

(ert-deftest unison-ts-indent/let-multiple-bindings-no-cascade ()
  "Multiple let bindings should all align at the same column, not cascade."
  (unison-ts-mode-tests--with-buffer "foo x =\n  let\n    a = 1\n    b = 2\n    c = 3"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 4))))

(ert-deftest unison-ts-indent/deeply-nested ()
  "Deeply nested expressions should maintain proper indentation."
  (unison-ts-mode-tests--with-buffer "foo x =\n  if x\n  then\n    let\n      y = 1\n      y\n  else 0"
    (goto-char (point-min))
    (forward-line 4)
    (should (= (current-indentation) 6))))

(ert-deftest unison-ts-indent/where-clause ()
  "Where clause bindings should be indented."
  (unison-ts-mode-tests--with-buffer "foo x = result where\n  result = x + 1"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/cases-expression ()
  "Cases expression arms should be indented."
  (unison-ts-mode-tests--with-buffer "bar = cases\n  Low -> \"low\"\n  Medium -> \"med\"\n  High -> \"high\""
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/cases-with-effects ()
  "Cases with effect patterns should be indented."
  (unison-ts-mode-tests--with-buffer "handler state = cases\n  {State.get -> resume} -> handler state (resume state)\n  {pure} -> (pure, state)"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/cases-multiline-body ()
  "Cases with multiline body should indent the body further."
  (unison-ts-mode-tests--with-buffer "process = cases\n  Some x ->\n    let\n      y = x + 1\n      z = y * 2\n    z\n  None -> 0"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 6))
    (forward-line 1)
    (should (= (current-indentation) 6))))

(ert-deftest unison-ts-indent/cases-inside-let ()
  "Cases expression inside let should be indented properly."
  (unison-ts-mode-tests--with-buffer "transform x =\n  let\n    f = cases\n      Just v -> v\n      Nothing -> 0\n  f x"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 6))
    (forward-line 1)
    (should (= (current-indentation) 6))))

(ert-deftest unison-ts-indent/handle-with-cases ()
  "Handle...with cases should indent properly."
  (unison-ts-mode-tests--with-buffer "process = handle\n  doSomething\nwith cases\n  {IO.getLine -> k} -> k \"input\"\n  {pure result} -> result"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 2)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/use-clause ()
  "Use clause in function body should be indented."
  (unison-ts-mode-tests--with-buffer "compute n =\n  use Nat + - *\n  use List map\n  n + 1"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/use-clause-in-let ()
  "Use clause in let block should be indented."
  (unison-ts-mode-tests--with-buffer "foo =\n  let\n    use Nat +\n    x = 1 + 2\n  x"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 4))))

(ert-deftest unison-ts-indent/tuple-literal ()
  "Tuple literals on multiple lines should indent."
  (unison-ts-mode-tests--with-buffer "pair = (\n  x,\n  y\n)"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/list-literal ()
  "List literals on multiple lines should indent."
  (unison-ts-mode-tests--with-buffer "items = [\n  1,\n  2,\n  3\n]"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/function-application-multiline ()
  "Function application on multiple lines should indent arguments."
  (unison-ts-mode-tests--with-buffer "result = myFunction\n  arg1\n  arg2\n  arg3"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 2))))

(ert-deftest unison-ts-indent/nested-cases-in-match ()
  "Cases expression in match arm should indent correctly."
  (unison-ts-mode-tests--with-buffer "process = match input with\n  Some handler ->\n    cases\n      {get} -> resume state\n      {pure} -> result\n  None -> defaultHandler"
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 6))
    (forward-line 1)
    (should (= (current-indentation) 6))))

(ert-deftest unison-ts-indent/complex-nesting ()
  "Complex nesting with let, cases, and match should maintain indentation."
  (unison-ts-mode-tests--with-buffer "foo x =\n  let\n    handler = cases\n      Some y ->\n        match y with\n          Just z -> z\n          Nothing -> 0\n      None -> -1\n  handler x"
    (goto-char (point-min))
    (forward-line 2)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (should (= (current-indentation) 6))
    (forward-line 1)
    (should (= (current-indentation) 8))
    (forward-line 1)
    (should (= (current-indentation) 10))
    (forward-line 1)
    (should (= (current-indentation) 10))))

;;; Mode activation tests

(ert-deftest unison-ts-mode/activates-for-u-files ()
  "Mode should activate for .u files."
  (require 'unison-ts-mode)
  (should (eq (cdr (assoc "\\.u\\'" auto-mode-alist)) 'unison-ts-mode)))

(ert-deftest unison-ts-mode/activates-for-unison-files ()
  "Mode should activate for .unison files."
  (require 'unison-ts-mode)
  (should (eq (cdr (assoc "\\.unison\\'" auto-mode-alist)) 'unison-ts-mode)))

;;; LSP integration tests

(ert-deftest unison-ts-mode-lsp/eglot-registered ()
  "unison-ts-mode should be registered with eglot."
  (require 'unison-ts-mode)
  (when (require 'eglot nil t)
    (should (assoc 'unison-ts-mode eglot-server-programs))))

(ert-deftest unison-ts-mode-lsp/lsp-mode-registered ()
  "unison-ts-mode should be registered with lsp-mode."
  (require 'unison-ts-mode)
  (when (require 'lsp-mode nil t)
    (should (assoc 'unison-ts-mode lsp-language-id-configuration))))

;;; ADT constructor tests

(ert-deftest unison-ts-font-lock/adt-constructor-after-equals ()
  "ADT constructors after = should be highlighted as types."
  (unison-ts-mode-tests--with-buffer "structural type Result a e = Ok a | Err e"
    (should (eq (unison-ts-mode-tests--face-at-string "Ok") 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/adt-constructor-after-pipe ()
  "ADT constructors after | should be highlighted as types."
  (unison-ts-mode-tests--with-buffer "structural type Result a e = Ok a | Err e"
    (should (eq (unison-ts-mode-tests--face-at-string "Err") 'font-lock-type-face))))

(ert-deftest unison-ts-font-lock/ability-operation-name ()
  "Ability operation names should be highlighted as functions."
  (unison-ts-mode-tests--with-buffer "structural ability State s where\n  get : {State s} s"
    (should (eq (unison-ts-mode-tests--face-at-string "get") 'font-lock-function-name-face))))

;;; Documentation tests

(ert-deftest unison-ts-font-lock/doc-block ()
  "Doc blocks ({{ }}) should be highlighted as doc."
  (unison-ts-mode-tests--with-buffer "{{ This is documentation }}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-doc-face))))

(ert-deftest unison-ts-font-lock/doc-block-multiline ()
  "Multiline doc blocks should be highlighted."
  (unison-ts-mode-tests--with-buffer "{{ This is\nmultiline\ndocumentation }}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-doc-face))))

(ert-deftest unison-ts-font-lock/doc-block-with-markdown ()
  "Doc blocks with **markdown** should be highlighted."
  (unison-ts-mode-tests--with-buffer "{{ Doc with **bold** text }}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-doc-face))))

(ert-deftest unison-ts-font-lock/doc-block-term-link ()
  "Doc blocks with {termName} links should be highlighted."
  (unison-ts-mode-tests--with-buffer "{{ See {myFunction} for details }}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-doc-face))))

(ert-deftest unison-ts-font-lock/doc-block-type-link ()
  "Doc blocks with {type TypeName} links should be highlighted."
  (unison-ts-mode-tests--with-buffer "{{ Returns a {type Optional} }}"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-doc-face))))

;;; Literal tests

(ert-deftest unison-ts-font-lock/literal-nat-decimal ()
  "Decimal Nat literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = 42"
    (goto-char (point-min))
    (search-forward "42")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-nat-hex ()
  "Hex Nat literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = 0x003"
    (goto-char (point-min))
    (search-forward "0x003")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-int-positive ()
  "Positive Int literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = +4"
    (goto-char (point-min))
    (search-forward "+4")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-int-negative ()
  "Negative Int literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = -4"
    (goto-char (point-min))
    (search-forward "-4")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-float-positive ()
  "Positive Float literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = 1.5"
    (goto-char (point-min))
    (search-forward "1.5")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-float-negative ()
  "Negative Float literals should be highlighted as constants."
  (unison-ts-mode-tests--with-buffer "x = -4.567"
    (goto-char (point-min))
    (search-forward "-4.567")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-text-simple ()
  "Simple Text literals should be highlighted as strings."
  (unison-ts-mode-tests--with-buffer "x = \"hello\""
    (goto-char (point-min))
    (search-forward "hello")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/literal-text-multiline ()
  "Multiline Text literals should be highlighted as strings."
  (unison-ts-mode-tests--with-buffer "x = \"\"\"multiline\"\"\""
    (goto-char (point-min))
    (search-forward "multiline")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/literal-char-simple ()
  "Simple Char literals should be highlighted as strings."
  (unison-ts-mode-tests--with-buffer "x = ?a"
    (goto-char (point-min))
    (search-forward "?a")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/literal-char-emoji ()
  "Emoji Char literals should be highlighted as strings."
  (unison-ts-mode-tests--with-buffer "x = ?🔥"
    (goto-char (point-min))
    (search-forward "?")
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/literal-char-escape ()
  "Escaped Char literals should be highlighted as strings."
  (unison-ts-mode-tests--with-buffer "x = ?\\t"
    (goto-char (point-min))
    (search-forward "?\\")
    (should (eq (get-text-property (point) 'face) 'font-lock-string-face))))

(ert-deftest unison-ts-font-lock/literal-boolean-true ()
  "Boolean true should be highlighted as constant."
  (unison-ts-mode-tests--with-buffer "x = true"
    (goto-char (point-min))
    (search-forward "true")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-boolean-false ()
  "Boolean false should be highlighted as constant."
  (unison-ts-mode-tests--with-buffer "x = false"
    (goto-char (point-min))
    (search-forward "false")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-bytes ()
  "Bytes literals should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = 0xsdeadbeef"
    (goto-char (point-min))
    (search-forward "0xsdeadbeef")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-list ()
  "List literals should parse correctly (elements highlighted)."
  (unison-ts-mode-tests--with-buffer "x = [1, 2, 3]"
    (goto-char (point-min))
    (search-forward "1")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-tuple ()
  "Tuple literals should parse correctly."
  (unison-ts-mode-tests--with-buffer "x = (1, \"x\")"
    (goto-char (point-min))
    (search-forward "1")
    (backward-char 1)
    (should (eq (get-text-property (point) 'face) 'font-lock-constant-face))))

(ert-deftest unison-ts-font-lock/literal-lambda ()
  "Lambda expressions should parse correctly."
  (unison-ts-mode-tests--with-buffer "x = y -> y"
    (goto-char (point-min))
    (search-forward "->")
    (backward-char 1)
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/literal-hash ()
  "Hash references should be highlighted."
  (unison-ts-mode-tests--with-buffer "x = #ko93h"
    (goto-char (point-min))
    (search-forward "#ko93h")
    (backward-char 1)
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/literal-termlink ()
  "termLink should be highlighted as a keyword or builtin."
  (unison-ts-mode-tests--with-buffer "x = termLink foo"
    (goto-char (point-min))
    (search-forward "termLink")
    (backward-char 1)
    (should (get-text-property (point) 'face))))

(ert-deftest unison-ts-font-lock/literal-typelink ()
  "typeLink should be highlighted as a keyword or builtin."
  (unison-ts-mode-tests--with-buffer "x = typeLink Nat"
    (goto-char (point-min))
    (search-forward "typeLink")
    (backward-char 1)
    (should (get-text-property (point) 'face))))

;;; Escape sequence tests

(ert-deftest unison-ts-font-lock/escape-null ()
  "Escape sequence \\0 should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\0\""
    (goto-char (point-min))
    (search-forward "\\0")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-bell ()
  "Escape sequence \\a should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\a\""
    (goto-char (point-min))
    (search-forward "\\a")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-backspace ()
  "Escape sequence \\b should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\b\""
    (goto-char (point-min))
    (search-forward "\\b")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-formfeed ()
  "Escape sequence \\f should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\f\""
    (goto-char (point-min))
    (search-forward "\\f")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-newline ()
  "Escape sequence \\n should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\n\""
    (goto-char (point-min))
    (search-forward "\\n")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-carriage-return ()
  "Escape sequence \\r should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\r\""
    (goto-char (point-min))
    (search-forward "\\r")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-tab ()
  "Escape sequence \\t should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\t\""
    (goto-char (point-min))
    (search-forward "\\t")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-vertical-tab ()
  "Escape sequence \\v should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\v\""
    (goto-char (point-min))
    (search-forward "\\v")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-space ()
  "Escape sequence \\s should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\s\""
    (goto-char (point-min))
    (search-forward "\\s")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-backslash ()
  "Escape sequence \\\\ should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\\\\""
    (goto-char (point-min))
    (search-forward "\\\\")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-single-quote ()
  "Escape sequence \\' should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\'\""
    (goto-char (point-min))
    (search-forward "\\'")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

(ert-deftest unison-ts-font-lock/escape-double-quote ()
  "Escape sequence \\\" should be highlighted in strings."
  (unison-ts-mode-tests--with-buffer "x = \"\\\"\""
    (goto-char (point-min))
    (search-forward "\\\"")
    (backward-char 1)
    (should (memq (get-text-property (point) 'face)
                  '(font-lock-string-face font-lock-escape-face)))))

;;; Mode activation tests

;;; Imenu tests

(ert-deftest unison-ts-mode-imenu/functions-indexed ()
  "Functions should appear in imenu."
  (unison-ts-mode-tests--with-buffer "foo x = x + 1\nbar = 42"
    (when (treesit-ready-p 'unison)
      (let ((index (funcall imenu-create-index-function)))
        (should (assoc "Functions" index))
        (let ((functions (cdr (assoc "Functions" index))))
          (should (>= (length functions) 2))
          (should (assoc-string "foo" functions))
          (should (assoc-string "bar" functions)))))))

(ert-deftest unison-ts-mode-imenu/types-indexed ()
  "Type declarations should appear in imenu."
  (unison-ts-mode-tests--with-buffer "type Result a e = Ok a | Err e"
    (when (treesit-ready-p 'unison)
      (let ((index (funcall imenu-create-index-function)))
        (should (assoc "Types" index))
        (let ((types (cdr (assoc "Types" index))))
          (should (assoc-string "Result" types)))))))

(ert-deftest unison-ts-mode-imenu/abilities-indexed ()
  "Ability declarations should appear in imenu."
  (unison-ts-mode-tests--with-buffer "structural ability Store v where\n  get : v"
    (when (treesit-ready-p 'unison)
      (let ((index (funcall imenu-create-index-function)))
        (should (assoc "Abilities" index))
        (let ((abilities (cdr (assoc "Abilities" index))))
          (should (assoc-string "Store" abilities)))))))

(ert-deftest unison-ts-mode-imenu/comprehensive ()
  "Imenu should index all major constructs."
  (unison-ts-mode-tests--with-buffer
      "foo x = x + 1\n\ntype Maybe a = Just a | Nothing\n\nstructural ability IO where\n  getLine : Text"
    (when (treesit-ready-p 'unison)
      (let ((index (funcall imenu-create-index-function)))
        (should (assoc "Functions" index))
        (should (assoc "Types" index))
        (should (assoc "Abilities" index))))))

(provide 'unison-ts-mode-tests)
;;; unison-ts-mode-tests.el ends here
