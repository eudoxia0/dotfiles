(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

;(global-font-lock-mode t) 
(mouse-wheel-mode t)
(setq require-final-newline t)

(tool-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;(set-face-attribute 'default nil :font "Terminus-9")
(set-face-attribute 'default nil :height 130)

(add-to-list 'load-path "~/.emacs.d/solarized")
(add-to-list 'load-path "~/.emacs.d/base16")
(add-to-list 'load-path "~/.emacs.d/plugins")

(require 'solarized-light-theme)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
