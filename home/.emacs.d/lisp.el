;;; Common Lisp options

;(load (expand-file-name "~/quicklisp/log4slime-setup.el"))
;(global-log4slime-mode 1)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)


(slime-setup '(slime-repl
               slime-asdf
               slime-fuzzy
               slime-banner
               slime-indentation
               slime-media))

(setq slime-enable-evaluate-in-emacs t)
