;;;; Configuration for Lisp modes

;;; Common Lisp

(setq sly-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)))

;;; Rainbow Parentheses

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;; Paredit

;(add-hook 'lisp-mode-hook 'paredit-mode)
