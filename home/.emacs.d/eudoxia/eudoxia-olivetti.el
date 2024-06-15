(add-hook 'olivetti-mode-hook
	  (lambda ()
			;; Set the buffer font to Times New Roman.
	    (set-frame-font "Times New Roman 18" t)
			;; Background colour.
      (set-face-attribute 'olivetti-fringe nil :background "gray91" :inherit 'default)
      (setq olivetti-body-width 70
            olivetti-style 'fancy)))

(provide 'eudoxia-olivetti)
