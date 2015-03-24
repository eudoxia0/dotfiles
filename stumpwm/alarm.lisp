(in-package :stumpwm)

(defun time-today (hour &optional (minute 0) (second 0))
  (multiple-value-bind (s m h day month year &rest rest) (get-decoded-time)
    (encode-universal-time second minute hour day month year)))

(defun set-alarm-today (fn hour &optional (minute 0) (second 0))
  (sb-thread:make-thread
   (lambda ()
     (sleep (- (time-today) (get-universal-time)))
     (funcall fn))))
