;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;                                        Bogdan Maryniuk

;; Fire up server
;(require 'server)
;;Start emacs server in running GUI and not already running
;(setq server-socket-dir "/tmp/emacs-shared")
;(if (display-graphic-p)
;    (unless (server-running-p)
;      (server-start)))

(add-hook 'after-save-hook 
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

;;; Themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-safe-themes t)

(load-theme 'dichromacy t)

;;; Basic interface options

(mouse-wheel-mode t)
(setq require-final-newline t)

(cua-mode 1)

;; Hide scrollbars
(scroll-bar-mode -1)
;; Hide toolbar
(tool-bar-mode -1)
(menu-bar-mode 0)
(global-linum-mode 1)
;; Empty scratch buffer
(setq initial-scratch-message "")
;; Don't show startup screen
(setq inhibit-splash-screen t)
;; Always y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default cursor-type 'bar)
;; Show column number
(setq column-number-mode  t)

;; Always use spaces when indenting (unless overridden for buffer)
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

;; Font options
(set-face-attribute 'default nil :font "Inconsolata")
(set-face-attribute 'default nil :height 100)

;;; Keybindings

(global-set-key (kbd "<f8>")
                (lambda()(interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-.") 'magit-status)

;;; Other files

(load (expand-file-name "~/.emacs.d/pack.el"))
(load (expand-file-name "~/.emacs.d/opt.el"))
(load (expand-file-name "~/.emacs.d/lisp.el"))
