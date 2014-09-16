;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;                                        Bogdan Maryniuk

(load (expand-file-name "~/.emacs.d/config.el"))
(load (expand-file-name "~/.emacs.d/pack.el"))
(load (expand-file-name "~/.emacs.d/lisp.el"))
(load (expand-file-name "~/.emacs.d/opt.el"))

;; Package-specific options
(load (expand-file-name "~/.emacs.d/setup-org.el"))

(load (expand-file-name "~/.emacs.d/key.el"))
