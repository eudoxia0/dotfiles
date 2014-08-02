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

(defcommand open-selection-browser () ()
  "from http://www.mygooglest.com/fni/stumpwm.html
   Get the X selection and order the GUI browser to open it."
  (run-shell-command (concatenate 'string
                                  +www-browser+
                                  " \""
                                  (get-x-selection)
                                  "\"")))
