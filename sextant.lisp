;;;;
;;;; Configuration for host "sextant"
;;;;
(in-package :cl-user)
(defpackage sextant
  (:use :cl :lcm :common)
  (:export :sextant))
(in-package :sextant)

;;; Constants.

(defparameter +packages+
  (list
   "xterm"
   "git"
   "rclone"
   "emacs"
   "keepassx"
   "spectrwm"
   "make"
   "gcc"
   "pcmanfm"
   "calibre"
   "scrot"
   "xfonts-terminus"
   "xcape"
   "xsecurelock"
   "dmenu"
   "rxvt-unicode"
   "acpi"
   "lxrandr"
   "nvidia-prime"
   "fonts-inconsolata"
   "sbcl"
   "curl"
   "pavucontrol"
   "whois"
   "expect"
   ))

;;; Configuration class.

(defun make-file (path)
  (make-file-component :title path
                       :source (format nil "sextant/~A" path)
                       :target (native-path (format nil "~~/~A" path))))

(defconfig sextant
  :secrets ()
  :components (((config sextant) vault)
               (declare (ignore config vault))
               (list
                (make-instance 'apt-one-way-component
                               :title "Install apt packages"
                               :packages +packages+)
                (make-file ".XCompose")
                (make-file ".bashrc")
                (make-file ".garglkrc")
                (make-file ".xscreensaver")
                (make-file ".xsession")
                (make-file ".emacs.d/init.el")
                (make-file ".local/bin/battery.sh")
                (make-file ".local/bin/embed_fonts.sh")
                (make-file ".local/bin/ordinal.py")
                (make-file ".local/bin/poetry.py")
                (make-file ".local/bin/rotate_wallpaper.sh")
                (make-file ".local/bin/ssd_rsync_backup.sh")
                (make-file ".local/bin/sync_from_cloud.sh")
                (make-file ".local/bin/sync_to_cloud.sh")
                )))
