;;;; -*- lexical-binding: t -*-

;; Defer GC during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Reset after startup:
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; No tool bar.
(tool-bar-mode -1)

;; No menu bar.
(menu-bar-mode -1)

;; Do not discretize the window size. Permits smooth resizing.
(setq frame-resize-pixelwise t)
