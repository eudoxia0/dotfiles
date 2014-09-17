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

;; Flycheck
(add-hook 'nxml-mode-hook 'flycheck-enable-except-on-temp-buffers)
