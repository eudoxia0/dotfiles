(in-package :stumpwm)

;;;; Commands

;;; Applications

(defcommand emacs () ()
  (run-or-raise "sh .scripts/start-emacs.sh" '(:class "emacs")))

(defcommand browser () ()
  (run-or-raise +www-browser+ '(:class "browser")))

(defcommand tor-browser () ()
  (run-or-raise "tor-browser-en" '(:class "tor-browser")))

(defcommand torrent () ()
  (run-or-raise "transmission-gtk" '(:class "torrent")))

(defcommand library () ()
  (run-or-raise "calibre" '(:class "library")))

(defcommand im () ()
  (run-or-raise "pidgin" '(:class "im")))

(defcommand music () ()
  (run-or-raise "audacious" '(:class "music")))

(defcommand fm () ()
  (run-or-raise "pcmanfm" '(:class "fm")))

(defcommand terminal () ()
  (run-or-raise +terminal+ '(:class "term")))

;;; System

(defcommand screensaver () ()
  (run-or-raise "xscreensaver-command -lock" '(:class "screensaver")))

(defcommand battery () ()
  "docstring"
  (message "~A" (run-shell-command "acpi" t)))

(defcommand uptime () ()
  "Display the system uptime"
  (message "~A" (run-shell-command "uptime" t)))

;;; Input

(defcommand send-click (times)
  ((:number "Number of clicks to send: "))
  "Send a simulated click."
  (loop repeat times do
    (send-click)))

(defun send-string (str)
  (run-shell-command (format nil "xdotool type ~S" str)))

(defcommand send-string (text)
  ((:string "Text to send: "))
  "Send a string to input."
  (send-string text))

(defcommand special (name)
    ((:string "Name: "))
  (let ((string (gethash name thorn:*character-table*)))
    (if string
        (send-string string))))

;;; Virtual Machines

(defun base-cmd (str)
  (concatenate 'string "vboxmanage list " str))

(defparameter +list-vms-command+ (base-cmd "vms"))
(defparameter +list-running-vms-command+ (base-cmd "runningvms"))

(defun process-vm-list (str)
  (mapcar
   #'(lambda (line)
       (subseq line 0 (position #\Space line)))
   (split-string str (string #\Newline))))

(defcommand vms () ()
  "List all VirtualBox VMs"
  (let ((list (process-vm-list
               (run-shell-command +list-vms-command+ t))))
  (message "~{~A~&~}" list)))

(defcommand running-vms () ()
  "List running VirtualBox VMs."
  (let ((list (process-vm-list
               (run-shell-command +list-running-vms-command+ t))))
    (message "~{~A~&~}" list)))
