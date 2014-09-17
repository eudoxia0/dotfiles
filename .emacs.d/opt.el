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
