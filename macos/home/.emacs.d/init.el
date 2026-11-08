;; -*- lexical-binding: t; -*-

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
