;;;; austral-mode.el - Major mode for editing Austral source files.
;;;;
;;;; Copyright (C) 2021-2023 Fernando Borretti
;;;;
;;;; Author: Fernando Borretti <fernando@borretti.me>
;;;; Maintainer: Fernando Borretti <fernando@borretti.me>
;;;; Created: 24 July, 2021
;;;; Version: 0.0.1
;;;; Keywords: languages
;;;;

(require 'cl-lib)

;;;; Constants

(defconst austral-keywords
  '("and" "or" "not" "module" "is" "body" "import" "as" "end" "constant" "type"
    "function" "generic" "record" "union" "case" "of" "when" "typeclass"
    "instance" "method" "if" "then" "else" "let" "while" "for" "do" "from"
    "to" "borrow" "return" "skip" "Free" "Linear" "Type" "Region"
    "pragma" "nil" "true" "false"))

(defconst austral-default-tab-width 4)

;;;; Syntax Highlighting

(defvar austral-font-lock-keywords
  (list
   ;; Comments
   (cons "--.*" font-lock-comment-face)
   ;; Keywords
   (cons (regexp-opt austral-keywords 'words) font-lock-keyword-face)
   ;; Identifiers
   (cons "\\<[[:alpha:]][_[:alnum:]]*\\>" font-lock-variable-name-face)))

;;;; Indentation

; etc.

;;;; Mode Definition

(define-derived-mode austral-mode fundamental-mode "Austral"
  "Major mode for editing Austral source text."
  (setq font-lock-defaults '((austral-font-lock-keywords)))
  (setq indent-line-function 'austral-indent-line))

(add-to-list 'auto-mode-alist '("\\.aui\\'" . austral-mode))
(add-to-list 'auto-mode-alist '("\\.aum\\'" . austral-mode))

(provide 'austral-mode)
