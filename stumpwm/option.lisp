(in-package :stumpwm)

;;;; General options

(set-prefix-key (kbd "s-w"))
(setf *mouse-focus-policy* :sloppy)
(setf *frame-number-map* "123456789")

(set-font "-xos4-terminus-medium-r-normal-*-12-120-*-*-*-60-*-u")

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

;;;; After start

(stumpwm::run-commands
 "gnew 1"
 "gnew 2"
 "gnew 3"
 "gnew 4"
 "gnew 5"
 "gnew 6"
 "gnew 7"
 "gnew 8"
 "gnew-float floating"
 "gselect 1"
 "gaps")
