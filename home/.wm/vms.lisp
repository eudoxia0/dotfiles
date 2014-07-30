(in-package :stumpwm)

(defun base-cmd (str)
  (concatenate 'string "vboxmanage list "))

(defparameter +list-vms-command+ (base-cmd "vms"))
(defparameter +list-running-vms-command+ (base-cmd "runningvms"))

(defun process-vm-list (str)
  (mapcar
   #'(lambda (line)
       (let* ((vm-id (first (split-string line " ")))
              (split (split-string vm-id "_"))
              ;; We don't care about the last two items
              (vm-name (subseq split 0 (- (length split) 2)))))
       (format nil "~{~A_~}" vm-name))
   (split-string str "\n")))

(defun list-vms ()
  (process-vm-list
   (run-shell-command +list-vms-command+ t)))

(defun list-running-vms ()
  (process-vm-list
   (run-shell-command +list-running-vms-command+ t)))
