;;;; Keybindings


(global-set-key (kbd "<f8>")
                (lambda()(interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-.") 'magit-status)
