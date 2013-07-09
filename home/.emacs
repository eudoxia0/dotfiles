(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "ccl")

(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

;(global-font-lock-mode t) 
(mouse-wheel-mode t)
(setq require-final-newline t)
(if (> emacs-major-version 20)
  (tool-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

;(set-face-attribute 'default nil :font "Terminus-9")

