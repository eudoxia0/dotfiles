(in-package :stumpwm)

;;;; General options

(set-prefix-key (kbd "s-w"))
(setf *mouse-focus-policy* :sloppy)
(setf *frame-number-map* "123456789")

(setq +terminal+ "urxvt")
(defparameter +www-browser+ "chromium")

;;;; Notifications

(defun echo-urgent-window (target)
  (message "Attention: ~A" (window-title target)))

(add-hook *urgent-window-hook* 'echo-urgent-window)

;;;; Theme

(set-fg-color "SteelBlue")
(set-bg-color "Black")
(set-border-color "Gray30")
(set-msg-border-width 0)
(setf *message-window-padding* 20)
(setf *maxsize-border-width* 0)
(setf *normal-border-width* 1)
(setf *window-border-style* :tight)
(set-focus-color "MidnightBlue")
(set-unfocus-color "Black")
(set-frame-outline-width 0)

;; e5d7a1 : dark yellow
;; ad3208 : red

;;;; Autostart

(stumpwm::run-commands
 "gnew web"
 "gnew term"
 "gnew edit"
 "gnew im"
 "gnew file"
 "gnew mail"
 "gselect web")
