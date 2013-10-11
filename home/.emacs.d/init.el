;;; Lisp

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

;;; Basic interface options

(mouse-wheel-mode t)
(setq require-final-newline t)

;; Hide scrollbars
(scroll-bar-mode -1)
;; Hide toolbar
(tool-bar-mode -1)
(menu-bar-mode 0)
;; Empty scratch buffer
(setq initial-scratch-message "")
;; Don't show startup screen
(setq inhibit-splash-screen t)
;; Always y/n o p
(fset 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Monaco")
(set-face-attribute 'default nil :height 100)

;;; Packages

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar packages
  '(org nav
    ;; Major modes
    yaml-mode clojure-mode d-mode textile-mode markdown-mode gnuplot-mode
    erlang haskell-mode    
    ;; Other plugins
    rainbow-delimiters))

(defun packages-installed-p ()
  (not (memq 'nil (mapcar (lambda (p) (package-installed-p p)) packages))))

(unless (packages-installed-p)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'packages)

;;; Package specific options

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;;; Modes

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.d$" . d-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;; Load themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'dichromacy t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
