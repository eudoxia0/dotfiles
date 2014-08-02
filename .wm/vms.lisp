(in-package :stumpwm)

(defun base-cmd (str)
  (concatenate 'string "vboxmanage list " str))

(defparameter +list-vms-command+ (base-cmd "vms"))
(defparameter +list-running-vms-command+ (base-cmd "runningvms"))

(defun process-vm-list (str)
  (mapcar
   #'(lambda (line)
       (subseq line 0 (position #\Space line)))
   (split-string str (string #\Newline))))

(defun list-vms ()
  (process-vm-list
   (run-shell-command +list-vms-command+ t)))

(defun list-running-vms ()
  (process-vm-list
   (run-shell-command +list-running-vms-command+ t)))

(defcommand vms () ()
  "List all VirtualBox VMs"
  (message "~{~A~&~}" (list-vms)))

(defcommand running-vms () ()
  "List running VirtualBox VMs."
  (message "~{~A~&~}" (list-running-vms)))
