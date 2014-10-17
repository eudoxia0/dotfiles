;;;; Emacs configuration

(require 'cl)
(require 'whitespace)

;; Compile files after saving them
(add-hook 'after-save-hook 
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

;;; Interface

(mouse-wheel-mode t)
(setq require-final-newline t)

(cua-mode 1)

(windmove-default-keybindings)

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Fill paragraph to 80
(setq-default fill-column 80)
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
;; Get rid of overwrite mode because i sometimes
;; enable it by accident
(put 'overwrite-mode 'disabled t)
;; Always y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default cursor-type 'bar)
;; Show column number
(setq column-number-mode  t)

;; Always use spaces when indenting (unless overridden for buffer)
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

;; Font options
(set-face-attribute 'default nil :font "Inconsolata")
(set-face-attribute 'default nil :height 100)

;;; Functionality
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default show-trailing-whitespace t)
