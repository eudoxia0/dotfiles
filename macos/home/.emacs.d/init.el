;; -*- lexical-binding: t; -*-

;;;
;;; General
;;;

;; This is needed to prevent some fuckass random-ass warning messages
;; about JIT compilation for some reason.
(setenv "MACOSX_DEPLOYMENT_TARGET" "27.0")

;; Don't create backup~ files.
(setq make-backup-files nil)

;; Don't create #autosave# files.
(setq auto-save-default nil)

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Get rid of overwrite mode because sometimes I enable it by accident.
(put 'overwrite-mode 'disabled t)

;; Disable things I often invoke by mistake.
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "C-t"))

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set the default tab width to 4.
(setq-default tab-width 4)

;; Persist minibuffer history.
(savehist-mode 1)

;; Remember cursor positions.
(save-place-mode 1)

;; Remember recently opened files.
(recentf-mode 1)

;; Reload externally changed files.
(global-auto-revert-mode 1)

;; Use Y or N instead of yes or no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;;
;;; Package management
;;;

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

;; Automatically install any package referenced by a `use-package' form that
;; isn't already installed.
(setq use-package-always-ensure t)

;;;
;;; Splash Screen
;;;

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

;;;
;;; UI
;;;

;; Line numbers everywhere.
(global-display-line-numbers-mode 1)

;; Don't blink the cursor.
(blink-cursor-mode 0)

;; Fill paragraphs to 80 columns.
(setq-default fill-column 80)

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Font.
(let ((font "Monaco-14"))
  (set-face-attribute 'default nil :font font)
  (set-face-attribute 'mode-line nil :font font))

;;;
;;; Mode Line
;;;

;; Track the column number.
(setq column-number-mode t)

;;;
;;; Mode: Markdown
;;;

(use-package markdown-mode
  :config
  (setq markdown-mouse-follow-link nil))
