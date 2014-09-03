(in-package :stumpwm)

;;;; General options

(set-prefix-key (kbd "s-w"))
(setf *mouse-focus-policy* :sloppy)
(setf *frame-number-map* "123456789")

(set-font "-xos4-terminus-medium-r-normal-*-12-120-*-*-*-60-*-u")

(setq +terminal+ "urxvt")
(defparameter +www-browser+ "chromium")

;;;; Notifications

(defun echo-urgent-window (target)
  (message "Attention: ~A" (window-title target)))

(add-hook *urgent-window-hook* 'echo-urgent-window)

;;;; Theme

(setf *normal-border-width* 0
      *maxsize-border-width* 0
      *transient-border-width* 0
      *window-border-style* :tight)

(set-win-bg-color "White")
(set-focus-color "MidnightBlue")
(set-unfocus-color "Black")

(set-border-color "Black")

(set-msg-border-width 0)
(set-frame-outline-width 0)

(set-normal-gravity :center)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

;;;; Autostart

(stumpwm::run-commands
 "gnew web"
 "gnew term"
 "gnew edit"
 "gnew im"
 "gnew file"
 "gnew mail"
 "gselect web")
