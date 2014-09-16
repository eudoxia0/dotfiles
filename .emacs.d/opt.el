;;;; Emacs options

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

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; Projectile
(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;; Highlight indentation
(require 'highlight-indentation)
(add-hook 'prog-mode
           (lambda () (highlight-indentation-current-column-mode)))

;; Powerline
(require 'powerline)

;; Web-mode
(require 'web-mode)
(setq web-mode-engines-alist '(("erb" . "\\.eco\\'")))

;; CEDET

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Flycheck
(add-hook 'nxml-mode-hook 'flycheck-enable-except-on-temp-buffers)

;; Flyspell
(setq ispell-program-name "hunspell")
(require 'rw-hunspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'nxml-mode-hook 'flyspell-prog-mode)

;; nxml

(setq nxml-slash-auto-complete-flag t)

;; dot mode

(setq dot-tab-width 2)
(defvaralias 'dot-indent-level 'dot-tab-width)

;;; Rainbow Parentheses

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#00877C" :weight normal))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b58900" :weight normal))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#6c71c4" :weight normal))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#cb4b16" :weight normal))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#859900" :weight normal))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#b58900" :weight normal))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#6c71c4" :weight normal))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#cb4b16" :weight normal))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#859900" :weight normal)))))

;;; Paredit

(add-hook 'lisp-mode-hook 'paredit-mode)

;;;; Mode file mapping

(setq auto-mode-alist
      (append '(("\\.md\\'" . markdown-mode)
		("\\.\\(yaml\\|yml\\)$" . yaml-mode)
		("\\.d$" . d-mode)
		("\\.clj$" . clojure-mode)
		("\\.textile$" . textile-mode)
		("\\.gp$" . gnuplot-mode)
		("\\.sql$" . sql-mode)
		("\\.\\(rb\\|Gemfile\\|Vagrantfile\\|Rakefile\\)$" . enh-ruby-mode)
		("\\.hs$" . haskell-mode)
                ("\\.dot$" . graphviz-dot-mode)
                ("\\.doc$" . adoc-mode)
                ;; Web modes
                ("\\.html$" . web-mode)
                ("\\.tmpl$" . web-mode)
                ("\\.eco$" . web-mode)
                ("\\.scss$" . sass-mode))
	    auto-mode-alist))

;;;; Themes

(setq custom-safe-themes t)
(color-theme-initialize)

(load-theme 'qsimpleq t)
