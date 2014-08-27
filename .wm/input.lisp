(in-package :stumpwm)

;;;; Send fake clicks or strings

(defun send-click ()
  (run-shell-command "xdotool click 1"))

(defun send-string (str)
  (run-shell-command (format nil "xdotool type ~S" str)))
