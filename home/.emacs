;;; Lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

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
    yaml-mode clojure-mode d-mode textile-mode markdown-mode))

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

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.d$" . d-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))

;;; Load themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'dichromacy t)

