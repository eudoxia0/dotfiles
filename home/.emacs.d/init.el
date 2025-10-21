;;;; -*- lexical-binding: t -*-

;;;;
;;;; Package Management
;;;;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar eudoxia-package-list
  '(olivetti
    aircon-theme
    markdown-mode
    treemacs
    magit
    nix-mode
    rust-mode
    unfill
    yaml-mode
    sly))

(dolist (package eudoxia-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;
;;;; General
;;;;

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Use tabs instead of spaces.
(setq-default indent-tabs-mode nil)
;; Set the default tab width to 4.
(setq-default tab-width 4)
;; IDO mode for better completions.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; No backup~ files.
(setq make-backup-files nil)
;; No #autosave# files.
(setq auto-save-default nil)
;; Get rid of overwrite mode because sometimes I enable it by accident.
(put 'overwrite-mode 'disabled t)

;; Disable the arrow keys.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "<end>"))

;; Disable things I often invoke by mistake.
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "C-l"))

;;;;
;;;; Mode: Common Lisp
;;;;

(setq inferior-lisp-program "sbcl")

;;;;
;;;; Mode: Markdown
;;;;

(setq markdown-mouse-follow-link nil)

;;;;
;;;; Mode Line
;;;;

;; Track the column number.
(setq column-number-mode t)

;;;;
;;;; Mode: Olivetti
;;;;

(add-hook 'olivetti-mode-hook
	  (lambda ()
			;; Set the buffer font to Times New Roman.
	    (set-frame-font "Times New Roman 18" t)
			;; Background colour.
      (set-face-attribute 'olivetti-fringe nil :background "gray91" :inherit 'default)
      (setq olivetti-body-width 70
            olivetti-style 'fancy)))

;;;;
;;;; Persian Input
;;;;

(defun eudoxia-persian ()
  "Use the Persian input method."
  (interactive)
  (set-input-method "farsi-transliterate-banan"))

;;;;
;;;; Special Character Input
;;;;

(defvar special-inputs
  '(;; Punctuation
    ("--" . "–")
    ("---" . "—")
    ("..." . "…"))
  "Map of shorthand to special characters.")

(defun eudoxia-special ()
  "Enter special characters."
  (interactive)
  (let* ((completions (mapcar #'car special-inputs))
         (input (completing-read "e/special: " completions))n
         (output (cdr (assoc input special-inputs))))
    (if output
        (insert output)
        (message "Not a shorthand: %s" input))))

;;;;
;;;; Splash Screen
;;;;

;; Set the contents of the scratch buffer.
(setq initial-scratch-message "")

;; No splash screen.
(setq inhibit-startup-screen t)

;; Words of encouragement, from the SLIME source code.
(defvar words-of-encouragement
  (list "Let the hacking commence!"
	"Hacks and glory await!"
	"Hack and be merry!"
	"Your hacking starts... NOW!"
	"May the source be with you!"
	"Take this REPL, brother, and may it serve you well."
	"Lemonodor-fame is but a hack away!"
	"Are we consing yet?"
	(format "%s, this could be the start of a beautiful program." (user-login-name)))
  "")

(defun random-words-of-encouragement ()
  "Return a string of hackerish encouragement. From the SLIME source code."
  (nth (random (length words-of-encouragement))
       words-of-encouragement))

;; Display a custom message in the echo area.
(defun display-startup-echo-area-message ()
  (message (random-words-of-encouragement)))

;;;;
;;;; Mode: Treemacs
;;;;

(global-set-key (kbd "<f8>") 'treemacs)

;;;;
;;;; UI
;;;;

;; Colour theme.
;(load-theme 'aircon t)

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
(set-face-attribute 'default nil :font "Monaco-15")
(set-face-attribute 'mode-line nil :font "Monaco-15")

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Highlight the current line.
(global-hl-line-mode +1)
(set-face-background 'hl-line "#eeeeee")

;; Do not discretize the window size. Permits smooth resizing.
(setq frame-resize-pixelwise t)

;;;;
;;;; Mode: XML
;;;;

(require 'nxml-mode)

(defun e/in-start-tag-p ()
  ;; Check that we're at the end of a start tag. From the source code of
  ;; `nxml-balanced-close-start-tag`.
  (let ((token-end (nxml-token-before))
	    (pos (1+ (point)))
	    (token-start xmltok-start))
    (or (eq xmltok-type 'partial-start-tag)
		(and (memq xmltok-type '(start-tag
					             empty-element
					             partial-empty-element))
		     (>= token-end pos)))))

(defun e/finish-element ()
  (interactive)
  (if (e/in-start-tag-p)
      ;; If we're at the end of a start tag like `<foo`, complete this to
      ;; `<foo></foo>`, then move the point between the start and end tags.
      (nxml-balanced-close-start-tag-inline)
      ;; Otherwise insert an angle bracket.
      (insert ">")))

(define-key nxml-mode-map (kbd ">") 'e/finish-element)

(defun e/nxml-newline ()
  "Insert a newline, indenting the current line and the newline appropriately in nxml-mode."
  (interactive)
  ;; Are we between an open and closing tag?
  (if (and (char-before) (char-after)
           (char-equal (char-before) ?>)
           (char-equal (char-after) ?<))
      ;; If so, indent it properly.
      (let ((indentation (current-indentation)))
        (newline)
        (indent-line-to (+ indentation 4))
        (newline)
        (indent-line-to indentation)
        (previous-line)
        (end-of-line))
    ;; Otherwise just insert a regular newline.
    (newline)))

(define-key nxml-mode-map (kbd "RET") 'e/nxml-newline)

(setq nxml-child-indent 4 nxml-attribute-indent 4)

;;;;
;;;; Unfill
;;;;

(global-set-key (kbd "<f9>") 'unfill-region)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e7ce09ff7426c9a290d06531edc4934dd05d9ea29713f9aabff834217dbb08e4" default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
