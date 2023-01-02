;;;;
;;;; Configuration for host "sextant"
;;;;
(in-package :cl-user)
(defpackage config
  (:use :cl :lcm :common)
  (:export :sextant :bullroarer))
(in-package :config)

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
   "texlive-fonts-extra"
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
   "dunst"
   "gnuplot"
   "htop"
   "libfuse2" ;; For the Mochi app.
   "jekyll"
   "gparted"
   "djview4"
   "meld"
   ))

;;; Configuration class.

(defun make-file (path &optional (executable nil))
  (make-file-component :title path
                       :source (format nil "files/~A" path)
                       :target (native-path (format nil "~~/~A" path))
                       :executable executable))

(defun list-of-components (&key laptop)
  (let ((components
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
           (if laptop
               (make-file ".local/bin/battery.sh" t)
               nil)
           (make-file ".local/bin/embed_fonts.sh" t)
           (make-file ".local/bin/ordinal.py" t)
           (make-file ".local/bin/poetry.py" t)
           (make-file ".local/bin/wallpaper.sh" t)
           (make-file ".local/bin/ssd_rsync_backup.sh" t)
           (make-file ".local/bin/sync_from_cloud.sh" t)
           (make-file ".local/bin/sync_to_cloud.sh" t)
           (make-file ".config/git/config")
           (make-file ".config/spectrwm/spectrwm.conf")
           (make-file ".config/x11/xmodmap")
           (make-file ".config/x11/xresources")
           (make-file "texmf/tex/latex/classicthesis.sty")
           (make-file "texmf/tex/latex/bussproofs.sty")
           (make-file "texmf/tex/latex/memoir/mem10.clo")
           (make-file "texmf/tex/latex/memoir/mem11.clo")
           (make-file "texmf/tex/latex/memoir/mem12.clo")
           (make-file "texmf/tex/latex/memoir/mem14.clo")
           (make-file "texmf/tex/latex/memoir/mem17.clo")
           (make-file "texmf/tex/latex/memoir/mem20.clo")
           (make-file "texmf/tex/latex/memoir/mem25.clo")
           (make-file "texmf/tex/latex/memoir/mem30.clo")
           (make-file "texmf/tex/latex/memoir/mem36.clo")
           (make-file "texmf/tex/latex/memoir/mem48.clo")
           (make-file "texmf/tex/latex/memoir/mem60.clo")
           (make-file "texmf/tex/latex/memoir/mem9.clo")
           (make-file "texmf/tex/latex/memoir/memhfixc.sty")
           (make-file "texmf/tex/latex/memoir/memoir.cls")
           (make-file "texmf/tex/latex/memoir/mempatch.sty"))))
    (remove-if #'null components)))

(defconfig sextant
  :secrets ()
  :components (((config sextant) vault)
               (declare (ignore config vault))
               (list-of-components :laptop t)))

(defconfig bullroarer
  :secrets ()
  :components (((config bullroarer) vault)
               (declare (ignore config vault))
               (list-of-components :laptop nil)))
