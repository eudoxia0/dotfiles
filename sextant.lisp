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

(defconfig sextant
  :secrets ()
  :components (((config sextant) vault)
               (declare (ignore config vault))
               (list
                (make-instance 'apt-one-way-component
                               :title "Install apt packages"
                               :packages +packages+)
                (make-file-component :title ".XCompose"
                                     :source #p"sextant/.XCompose"
                                     :target (native-path "~/.XCompose")))))
