;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;                                        Bogdan Maryniuk

;; Fire up server
(require 'server)
;;Start emacs server in running GUI and not already running
(setq server-socket-dir "/tmp/emacs-shared")
(if (display-graphic-p)
    (unless (server-running-p)
      (server-start)))

;;; Themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'dichromacy t)

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
;; Always y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default cursor-type 'bar)
;; Show column number
(setq column-number-mode  t)

;; Always use spaces when indenting (unless overridden for buffer)
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

;; Highlight line
(global-hl-line-mode)

;; Font options
(set-face-attribute 'default nil :font "Monaco")
(set-face-attribute 'default nil :height 100)

;;; Common Lisp options

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

;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar packages
  '(org nav
    ;; Major modes
    yaml-mode clojure-mode d-mode textile-mode markdown-mode gnuplot-mode
    erlang haskell-mode fsharp-mode sass-mode rainbow-mode ruby-mode
    ;; Other plugins
    rainbow-delimiters magit ecb))

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

(setq auto-mode-alist
      (append '(("\\.md\\'" . markdown-mode)
		("\\.\\(yaml\\|yml\\)$" . yaml-mode)
		("\\.d$" . d-mode)
		("\\.clj$" . clojure-mode)
		("\\.textile$" . textile-mode)
		("\\.gp$" . gnuplot-mode)
		("\\.sql$" . sql-mode)
		("\\.\\(rb\\|Gemfile\\|Rakefile\\)$" . ruby-mode)
		("\\.hs$" . haskell-mode))
	      auto-mode-alist))

;;; Keybindings

(global-set-key (kbd "<f8>")
                (lambda()(interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-.") 'magit-status)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
