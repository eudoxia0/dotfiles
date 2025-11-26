;;;; -*- lexical-binding: t -*-

;;;
;;; Startup
;;;

;; Defer GC during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Reset after startup:
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;;;
;;; General
;;;

;; No backup~ files.
(setq make-backup-files nil)

;; No #autosave# files.
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

;; No tool bar.
(tool-bar-mode -1)

;; No menu bar.
(menu-bar-mode -1)

;; Do not discretize the window size. Permits smooth resizing.
(setq frame-resize-pixelwise t)

;; Don't blink the cursor.
(blink-cursor-mode 0)

;; Fill paragraphs to 80 columns.
(setq-default fill-column 80)

;; Host predicates.
(defun rostam-p ()
  "Return t if running on rostam."
  (string= (system-name) "rostam"))

(defun ismene-p ()
  "Return t if running on ismene."
  (string= (system-name) "ismene"))

;; Font.
(let ((font-size (cond
                  ((rostam-p) "16")  ; 4K monitor
                  ((ismene-p) "12")  ; 1080p laptop
                  (t "15"))))        ; default
  (let ((font (format "Fira Code-%s" font-size)))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'mode-line nil :font font)))

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;;;
;;; Mode Line
;;;

;; Track the column number.
(setq column-number-mode t)

;;;
;;; Themes
;;;

(require 'cl-lib)

(defvar my-themes
  '(adwaita
    brin
    deeper-blue
    dichromacy
    dorsey
    fogus
    graham
    granger
    hickey
    junio
    kaolin-aurora
    kaolin-blossom
    kaolin-breeze
    kaolin-bubblegum
    kaolin-dark
    kaolin-eclipse
    kaolin-galaxy
    kaolin-light
    kaolin-mono-dark
    kaolin-mono-light
    kaolin-ocean
    kaolin-shiva
    kaolin-temple
    kaolin-valley-dark
    kaolin-valley-light
    leuven
    leuven-dark
    manoj-dark
    mccarthy
    misterioso
    modus-operandi
    modus-vivendi
    moe-dark
    moe-light
    nano
    nano-dark
    nano-light
    odersky
    ritchie
    spolsky
    tango
    tango-dark
    tsdh-dark
    tsdh-light
    whiteboard
    wilson
    wombat
    wheatgrass))

(defvar my-default-theme 'kaolin-breeze)

(defvar my-current-theme my-default-theme)

(load-theme my-current-theme t)

(defun theme-cycle (direction)
  "Cycle through `my-themes` in DIRECTION (:next or :prev)."
  (interactive (list (intern (completing-read "Direction: " '(:next :prev) nil t))))
  (let* ((len (length my-themes))
         (delta (if (eq direction :prev) -1 1))
         (cur-idx (or (cl-position my-current-theme my-themes :test #'eq) -1))
         (start-idx (if (= cur-idx -1) 0 cur-idx))
         (next-idx (mod (+ start-idx delta) len))
         (next (nth next-idx my-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (condition-case err
        (progn
          (load-theme next t)
          (setq my-current-theme next)
          (message "Theme: %s" next))
      (error
       (message "Failed to load theme %S: %s" next (error-message-string err))))))

(defun theme-next ()
  (interactive)
  (theme-cycle :next))

(defun theme-prev ()
  (interactive)
  (theme-cycle :prev))

(defun theme-reset ()
  (interactive)
  (setq my-current-theme my-default-theme)
  (load-theme my-current-theme t)
  (message "Theme: %s" my-current-theme))

(keymap-global-set "<f9>"    'theme-prev)
(keymap-global-set "<f10>"   'theme-next)
(keymap-global-set "C-<f10>" 'theme-reset)

;;;
;;; Sorting
;;;

(defun sort-buffer-lines ()
  "Sort all lines in the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (sort-lines nil (point-min) (point-max))))
  (message "Buffer lines sorted"))

(keymap-global-set "C-c s"   'sort-lines)
(keymap-global-set "C-c S"   'sort-buffer-lines)

;;;
;;; Mode: Markdown
;;;

(setq markdown-mouse-follow-link nil)

;;;
;;; Mode: Vertico
;;;

(use-package vertico
  :init
  (vertico-mode)
  :config
  (require 'vertico-directory)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

;;;
;;; Mode: Projectile
;;;

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;;;
;;; Mode: Treemacs
;;;

;; When you switch to a buffer, Treemacs displays only the directory for that
;; buffer's Projectile project.
(treemacs-project-follow-mode t)

(setq treemacs-collapse-dirs 0)

(keymap-global-set "C-c t" 'treemacs)

;;;
;;; Mode: Inform7
;;;

(require 'inform7)

;;;
;;; Mode: XCompose
;;;

(require 'xcompose-mode)

(add-to-list 'auto-mode-alist '("\\.XCompose\\'" . xcompose-mode))
(add-to-list 'auto-mode-alist '("\\.xcm\\'" . xcompose-mode))

;;;
;;; Mode: Lean4
;;;

(require 'lean4-mode)

;;;
;;; Mode: nXML
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
;;; Mode: eat
;;;

(use-package eat
  :config
  ;; Set nu as the default shell.
  (setq eat-shell "/usr/bin/env nu")

  ;; Disable trailing whitespace highlighting in eat.
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;;;
;;; Mode: cabal
;;;

(require 'cabal-mode)

;;;
;;; Mode: web-mode
;;;

(use-package web-mode
  :mode
  (("\\.jinja2\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.jinja2\\'"))))

;;;
;;; Quick Commit
;;;

(defun quick-commit ()
  "Stage all changes, commit with a prompt, and push to origin."
  (interactive)
  (let* ((branch (string-trim
                  (shell-command-to-string
                   "git rev-parse --abbrev-ref HEAD")))
         (commit-msg (read-string "Commit message: ")))
    (shell-command "git add -u")
    (shell-command (format "git commit -m %s"
                           (shell-quote-argument commit-msg)))
    (shell-command (format "git push origin %s" branch))
    (message "Committed and pushed to origin/%s" branch)))

(keymap-global-set "C-c ." 'quick-commit)
