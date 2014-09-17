;; Projectile
(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

;; Powerline
(require 'powerline)

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
