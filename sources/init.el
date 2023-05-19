;;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;;                                        Bogdan Maryniuk
;;;;

;;;;
;;;; Colour Theme
;;;;

(load-theme 'aircon t)

;;;;
;;;; User Interface
;;;;

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
;; Make the cursor a bar.
(setq-default cursor-type 'bar)
;; Set the cursor colour.
(set-cursor-color "#cccccc")
;; Fill paragraphs to 80 columns.
(setq-default fill-column 80)
;; Line numbers everywhere.
(global-linum-mode 1)
;; Get rid of overwrite mode because sometimes I enable it by accident.
(put 'overwrite-mode 'disabled t)
;; Default font.
(add-to-list 'default-frame-alist
             '(font . "Monaco-12"))
;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)
;; Highlight the current line.
(global-hl-line-mode +1)
(set-face-background 'hl-line "#eeeeee")

;;;;
;;;; Splash Screen
;;;;

;; Disable the messages buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Set the contents of the scratch buffer.
(setq initial-scratch-message "")

;; No splash screen.
(setq inhibit-startup-screen t)

;; Display a custom message in the echo area.

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
  "Words of encouragement, from the SLIME source code.")

(defun random-words-of-encouragement ()
  "Return a string of hackerish encouragement. From the SLIME source code."
  (nth (random (length words-of-encouragement))
       words-of-encouragement))

(defun display-startup-echo-area-message ()
  (message (random-words-of-encouragement)))

;;;;
;;;; Mode Line
;;;;

;; Track the column number.
(setq column-number-mode t)

;;;;
;;;; Behaviour
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

;;;;
;;;; Mode-Specific Configuration
;;;;

;;;
;;; SLIME
;;;

(setq inferior-lisp-program "sbcl")
;; When compilation fails, don't ask me whether to load the fasl file
;; anyways. Emacs is very fucky with modeline prompts and this always blocks the
;; rest of the UI and somehow breaks the window layout.
(setq slime-load-failed-fasl 'never)

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner))

;;;
;;; Olivetti
;;;

;; Set the buffer font to Times New Roman.
(add-hook 'olivetti-mode-hook
	  (lambda ()
	    (set-frame-font "Times New Roman 18" t)))

;;;
;;; NXML
;;;

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

;;;
;;; Markdown
;;;

;; Disable following links.
(setq markdown-mouse-follow-link nil)

;;;
;;; OCaml
;;;

(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;;;
;;;; Custom Commands
;;;;

(defun e/persian ()
  "Use the Persian input method."
  (interactive)
  (set-input-method "farsi-transliterate-banan"))

;;;;
;;;; Interaction
;;;;

;; Disable the mouse.
(require 'disable-mouse)
(global-disable-mouse-mode)

;; Disable the arrow keys.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; Disable things I often invoke by mistake.
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "C-l"))

;;;;
;;;; Custom Set Variables
;;;;
