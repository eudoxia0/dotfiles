;;;;
;;;; Brightness Control
;;;;
(in-package :stumpwm)

(defun brightness-get ()
  "Get the current brightness value from the external monitor using ddcutil."
  (let* ((output (run-shell-command "ddcutil getvcp 10" t))
         ;; Parse output like `VCP code 0x10 (Brightness): current value = 50, max value = 100`
         (current-pos (search "current value =" output)))
    (if current-pos
        (let ((start (+ current-pos (length "current value ="))))
          (multiple-value-bind (value end)
            (parse-integer output :start start :junk-allowed t)
            (or value 50)))
        ; Default to 50 if parsing fails
      50)))

(defun brightness-set (value)
  (let ((clamped-value (max 0 (min 100 value))))
    (run-shell-command (format nil "ddcutil setvcp 10 ~d" clamped-value))
    clamped-value))

(defun brightness-change (d)
  (let ((b (brightness-get)))
    (let ((b* (+ b d)))
      (brightness-set b*)
      (message "Brightness: ~d" b*))))

(defcommand brightness-up () ()
  "Increase brightness."
  (brightness-change 5))

(defcommand brightness-down () ()
  "Decrease brightness."
  (brightness-change -5))

(define-interactive-keymap ibrightness ()
  ((kbd "=") "brightness-up")
  ((kbd "-") "brightness-down"))

;; Interactively adjust brightness.
(defprefixkey "b" "ibrightness")
