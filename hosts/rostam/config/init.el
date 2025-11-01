;;;; -*- lexical-binding: t -*-

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

;;;
;;; Splash Screen
;;;

;; Set the contents of the scratch buffer.
(setq initial-scratch-message "")

;; No splash screen.
(setq inhibit-startup-screen t)

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

;; Font.
(let ((font "Fira Code-11"))
  (set-face-attribute 'default nil :font font)
  (set-face-attribute 'mode-line nil :font font))

;;;;
;;;; Mode: Markdown
;;;;

(setq markdown-mouse-follow-link nil)

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
    leuven
    leuven-dark
    manoj-dark
    mccarthy
    misterioso
    modus-operandi
    modus-vivendi
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
    wombat
    wheatgrass))

(defvar my-default-theme 'kaolin-galaxy)

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
;;; etc.
;;;

(add-to-list 'load-path "~/.emacs.d/eudoxia/")
(require 'inform7)
