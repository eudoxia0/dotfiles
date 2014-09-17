;;;; Spell checking

(setq ispell-program-name "hunspell")
(require 'rw-hunspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'nxml-mode-hook 'flyspell-prog-mode)
