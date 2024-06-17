;; Colour theme.
(load-theme 'aircon t)

;; No scroll bar.
(toggle-scroll-bar -1)

;; No tool bar.
(tool-bar-mode -1)

;; No menu bar.
(menu-bar-mode -1)

;; Stop with the annoying fucking bell on Mac OS.
(setq ring-bell-function 'ignore)

;; Don't blink the cursor.
(blink-cursor-mode 0)

;; Make the cursor a box.
(setq-default cursor-type 'box)

;; Cursor colour.
(defvar default-cursor-color "#cccccc")
(defvar modal-cursor-color "#ef5350")
(set-cursor-color default-cursor-color)

;; Fill paragraphs to 80 columns.
(setq-default fill-column 80)

;; Line numbers everywhere.
(global-display-line-numbers-mode 1)

;; Default fonts.
(set-face-attribute 'default nil :font "Inconsolata-15")
(set-face-attribute 'mode-line nil :font "Inconsolata-15")

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Highlight the current line.
(global-hl-line-mode +1)
(set-face-background 'hl-line "#eeeeee")

;; Do not discretize the window size. Permits smooth resizing.
(setq frame-resize-pixelwise t)

(provide 'eudoxia-ui)
