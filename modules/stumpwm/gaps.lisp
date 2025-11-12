;;;;
;;;; Useless Gaps
;;;;
;;;; Based on: https://github.com/stumpwm/stumpwm-contrib/tree/326725802fcedc2f8f28df91d4b548743da9e7bc/util/swm-gaps
;;;;
(in-package :stumpwm)

;; Head gaps run along the 4 borders of the monitor(s). NOTE: if you
;; have a systray (e.g. polybar) a positive value of this parameter
;; will clip the sides of it.
(defparameter *head-gaps-size* 0)
