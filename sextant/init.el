;;;;     "Show me your ~/.emacs and I will tell you who you are."
;;;;                                        Bogdan Maryniuk
;;;;
;;;; Sections:
;;;;
;;;;   1. Package Management
;;;;   2. Colour Theme
;;;;   3. User Interface
;;;;   4. Splash Screen
;;;;   5. Mode Line
;;;;   6. Behaviour
;;;;   7. Mode-Specific Configuration

;;;;
;;;; 1. Package Management
;;;;

(require 'package)

;; Use MELPA and MELPA-Stable exclusively.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(defvar my-packages
  '(olivetti       ; Distraction-free writing mode
    slime          ; Common Lisp IDE
    github-theme   ; GitHub theme
    sublime-themes ; Sublime themes
    tuareg         ; OCaml IDE
    merlin         ; OCaml error highlighting
    ))

;; Iterate over package list and install them.
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;;;
;;;; 2. Colour Theme
;;;;

(load-theme 'github t)

;;;;
;;;; 3. User Interface
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
(custom-set-faces
 '(default ((t (:background nil :family "Inconsolata" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))
;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)
;; Highlight the current line.
(global-hl-line-mode +1)

;;;;
;;;; 4. Splash Screen
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
;;;; 5. Mode Line
;;;;

;; Track the column number.
(setq column-number-mode t)

;; Set the mode line format.
(setq-default mode-line-format
	      (list
	       " "
	       '(:eval (propertize "%b" 'face 'bold))
	       " [%m] %p ("
	       '(line-number-mode "%l")
	       ","
	       '(column-number-mode "%c")
	       ")"))

;;;;
;;;; 6. Behaviour
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

;;;;
;;;; 7. Mode-Specific Configuration
;;;;

;;;
;;; SLIME
;;;

(setq inferior-lisp-program "sbcl")

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner))

;;;
;;; Olivetti
;;;

;; Set the buffer font to Times New Roman.
(add-hook 'olivetti-mode-hook
	  (lambda ()
	    (set-frame-font "Times New Roman 18" t)))

;;;;
;;;; Custom Modes
;;;;

;;;
;;; ConcordiaML
;;;

;;; Syntax highlighting

(defvar concordia-font-lock-keywords
  '(("\\\\begin{\\([a-zA-Z]+\\)}" 1 font-lock-function-name-face)
    ("\\\\end{\\([a-zA-Z]+\\)}" 1 font-lock-function-name-face)
    ("\\\\[a-zA-Z]+" . font-lock-keyword-face)
    ("@begin{\\([a-zA-Z]+\\)}" 1 font-lock-function-name-face)
    ("@end{\\([a-zA-Z]+\\)}" 1 font-lock-function-name-face)
    ("@[a-zA-Z]+" . font-lock-keyword-face)))

(define-derived-mode concordia-mode text-mode "Concordia"
  "Major mode for editing ConcordiaML files."
  (setq font-lock-defaults '((concordia-font-lock-keywords))))

;;;
;;; OCaml
;;;

(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;;;
;;;; Custom Set Variables
;;;;
