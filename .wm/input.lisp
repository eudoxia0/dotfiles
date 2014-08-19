(in-package :stumpwm)

(defun shell (str)
  (run-shell-command str t))

(defun send-click ()
  (shell (format nil "xte 'click 1'")))

(defun send-string (str)
  (shell (format nil "xte 'str ~A'" str)))

(defparameter +input-list+
  (mapcar #'(lambda (elem)
              (cons elem elem))
          (list "😸")))

(defun pick-input-preset ()
  (select-from-menu (current-screen) +input-list+))
