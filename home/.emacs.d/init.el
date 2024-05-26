;;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;;                                        Bogdan Maryniuk
;;;;

;;;;
;;;; Vendored Packages
;;;;

(let ((default-directory  "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory  "~/.emacs.d/vendor/treemacs/src/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'hydra)
(require 'olivetti)
(require 'aircon-theme)
(require 'markdown-mode)
(require 'cfrs)
(require 'posframe)
(require 'treemacs)
(require 'treemacs-mouse-interface)

;;;;
;;;; Colour Theme
;;;;

(load-theme 'aircon t)

;;;;
;;;; User Interface
;;;;

(defvar default-cursor-color "#cccccc")
(defvar modal-cursor-color "#ef5350")

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
;; Set the cursor colour.
(set-cursor-color default-cursor-color)
;; Fill paragraphs to 80 columns.
(setq-default fill-column 80)
;; Line numbers everywhere.
(global-display-line-numbers-mode 1)
;; Get rid of overwrite mode because sometimes I enable it by accident.
(put 'overwrite-mode 'disabled t)
;; Default fonts.
(set-face-attribute 'default nil :font "Monaco-13")
(set-face-attribute 'mode-line nil :font "Monaco-13")
;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)
;; Highlight the current line.
(global-hl-line-mode +1)
(set-face-background 'hl-line "#eeeeee")

;; Do not discretize the window size. Permits smooth resizing.
(setq frame-resize-pixelwise t)

;;;;
;;;; Splash Screen
;;;;

;; Disable the messages buffer.
;(setq-default message-log-max nil)
;(kill-buffer "*Messages*")

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
; (setq slime-load-failed-fasl 'never)

; (require 'slime-autoloads)
; (slime-setup '(slime-fancy slime-banner))

;;;
;;; Olivetti
;;;

;; Set the buffer font to Times New Roman.
(add-hook 'olivetti-mode-hook
	  (lambda ()
	    (set-frame-font "Times New Roman 18" t)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(olivetti-fringe ((t (:background "gray91" :inherit default)))))

(custom-set-variables
 '(olivetti-style 'fancy)
 '(olivetti-body-width 70))

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

(defvar special-inputs
  '(;; Punctuation
    ("--" . "–")
    ("---" . "—")
    ("..." . "…")
    ;; Spanish
    ("n~" . "ñ"))
  "Map of shorthand to special characters.")

(defun e/special ()
  "Enter special characters."
  (interactive)
  (let* ((completions (mapcar #'car special-inputs))
         (input (completing-read "e/special: " completions))n
         (output (cdr (assoc input special-inputs))))
    (if output
        (insert output)
        (message "Not a shorthand: %s" input))))

;;;;
;;;; Interaction
;;;;

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
;;;; Hydras
;;;;

(defhydra e/hydra-windows (:foreign-keys warn)
  "windows"
  ("x" delete-window)
  ("f" delete-other-windows)
  ("v" split-window-vertically)
  ("h" split-window-horizontally)
  ("o" other-window)
  (">" enlarge-window-horizontally)
  ("<" shrink-window-horizontally)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("r" (text-scale-increase 0))
  ("q" nil))

(global-set-key (kbd "C-v") 'e/hydra-windows/body)

(defhydra e/hydra-vi (:foreign-keys warn
                      :hint nil
                      :pre (set-cursor-color modal-cursor-color)
                      :post (set-cursor-color default-cursor-color))
  "modal editing"
  ("b" backward-char)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)

  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("N" (next-line 10))
  ("P" (previous-line 10))
  ;; Page Up
  ("<prior>" scroll-down-command)
  ;; Page Down
  ("<next>" scroll-up-command)

  ("w" kill-region)
  ("W" kill-ring-save)
  ("y" yank)

  ("s" isearch-forward)
  ("r" isearch-backward)
  ;("t" iy-go-to-char)
  ;("T" iy-go-to-char-backward)

  ("SPC" set-mark-command)
  ("G" goto-line)

  ("0" (digit-argument 0))
  ("1" (digit-argument 1))
  ("2" (digit-argument 2))
  ("3" (digit-argument 3))
  ("4" (digit-argument 4))
  ("5" (digit-argument 5))
  ("6" (digit-argument 6))
  ("7" (digit-argument 7))
  ("8" (digit-argument 8))
  ("9" (digit-argument 9))

  ("d" delete-char)
  ("D" kill-word)

  (":" execute-extended-command)
  ("x" replace-string)
  ("X" vr/query-replace)

  ("/" undo)

  ("B" switch-to-buffer)
  (">" next-buffer)
  ("<" previous-buffer)

  ("i" nil :color blue))

(global-set-key (kbd "<escape>") 'e/hydra-vi/body)

;;;;
;;;; Custom Set Variables
;;;;
