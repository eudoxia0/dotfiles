;; -*- mode: Lisp -*-
(in-package :stumpwm)

;;;; Constants

(defparameter +browser+ "firefox")

;;;; Commands

;;; Applications
(defcommand emacs () ()
  (run-or-raise "emacs" '(:class "emacs")))

(defcommand browser () ()
  (run-or-raise +browser+ '(:class "browser")))

(defcommand tor-browser () ()
  (run-or-raise "tor-browser-en" '(:class "tor-browser")))

(defcommand torrent () ()
  (run-or-raise "transmission-gtk" '(:class "torrent")))

(defcommand library () ()
  (run-or-raise "calibre" '(:class "library")))

(defcommand im () ()
  (run-or-raise "pidgin" '(:class "im")))

(defcommand music () ()
  (run-or-raise "clementine" '(:class "music")))

(defcommand fm () ()
  (run-or-raise "pcmanfm" '(:class "fm")))

;;; System
(defcommand screensaver () ()
  (run-or-raise "xscreensaver-command -lock" '(:class "screensaver")))

(defcommand battery () ()
  "Display battery information"
  (message "~A" (run-shell-command "acpi" t)))

(defcommand uptime () ()
  "Display the system uptime"
  (message "~A" (run-shell-command "uptime" t)))

;;; Input
(defcommand single-click () ()
  "Send a click."
  (run-shell-command "xdotool click 1"))

(defcommand middle-click () ()
  "Send a middle click."
  (run-shell-command "xdotool click 2"))

;;;; Keybindings

;;; Basic actions
(define-key *top-map* (kbd "s-Q") "quit")
(define-key *top-map* (kbd "s-R") "loadrc")

;;; Remove some preset keybindings
(undefine-key *root-map* (kbd "F1"))
(undefine-key *root-map* (kbd "F2"))
(undefine-key *root-map* (kbd "F3"))
(undefine-key *root-map* (kbd "F4"))
(undefine-key *root-map* (kbd "F5"))
(undefine-key *root-map* (kbd "F6"))
(undefine-key *root-map* (kbd "F7"))
(undefine-key *root-map* (kbd "F8"))
(undefine-key *root-map* (kbd "F9"))
(undefine-key *root-map* (kbd "F10"))

(undefine-key *root-map* (kbd "c")) ; Console
(undefine-key *root-map* (kbd "k")) ; Kill
(undefine-key *root-map* (kbd "!")) ; Run shell command

; Compound keybindings
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-e"))


;;; Workspaces
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-6") "gselect 6")
(define-key *top-map* (kbd "s-7") "gselect 7")
(define-key *top-map* (kbd "s-8") "gselect 8")
(define-key *top-map* (kbd "s-f") "gselect floating")


;;; Command keybindings


(define-key *root-map* (kbd "e") "emacs")


(define-key *top-map* (kbd "C-s-w") "browser")
(define-key *top-map* (kbd "C-s-o") "tor-browser")
(define-key *top-map* (kbd "C-s-t") "torrent")
(define-key *top-map* (kbd "C-s-b") "library")
(define-key *top-map* (kbd "C-s-i") "im")
(define-key *top-map* (kbd "C-s-m") "music")
(define-key *top-map* (kbd "C-s-f") "fm")


(define-key *top-map* (kbd "s-x") "screensaver")
(define-key *top-map* (kbd "s-q") "delete")
(define-key *top-map* (kbd "s-c") "exec xterm")
(define-key *top-map* (kbd "s-r") "exec")


(define-key *top-map* (kbd "s-e") "single-click")
(define-key *top-map* (kbd "s-v") "middle-click")


; Take a screenshot
(define-key *root-map* (kbd "Print") "exec scrot")


;;;; General options


(set-prefix-key (kbd "s-w"))
(setf *mouse-focus-policy* :sloppy)
(setf *frame-number-map* "123456789")


(set-font "-xos4-terminus-medium-r-normal-*-12-120-*-*-*-60-*-u")


;;;; Notifications


(defun echo-urgent-window (target)
  (message "Attention: ~A" (window-title target)))


(add-hook *urgent-window-hook* 'echo-urgent-window)

;;;; Theme


(setf *normal-border-width* 0
      *maxsize-border-width* 0
      *transient-border-width* 0
      *window-border-style* :tight)


(set-win-bg-color "White")
(set-focus-color "MidnightBlue")
(set-unfocus-color "Black")


(set-border-color "Black")


(set-msg-border-width 0)
(set-frame-outline-width 0)


(set-normal-gravity :center)
(set-maxsize-gravity :center)
(set-transient-gravity :center)


;;;; After start


(stumpwm::run-commands
 "gnew 1"
 "gnew 2"
 "gnew 3"
 "gnew 4"
 "gnew 5"
 "gnew 6"
 "gnew 7"
 "gnew 8"
 "gnew-float floating"
 "gselect 1"
 "gaps")


;; from https://github.com/lidstah/liddotfiles/blob/master/stumpwmrc_BLUEZ
(set-fg-color "#9BBBC6")
(set-bg-color "#191C23")
(set-border-color "#323246")
;;(set-focus-color "dimgray")
(set-focus-color "#506070")
(set-unfocus-color   "#191C23")
(set-win-bg-color    "#191C23")
(setf *colors* (list "#323246"          ; 0 black
                     "#5A7882" ; 1 red
                     "#8C8CA0"     ; 2 green
                     "#1E828C"        ; 3 yellow
                     "#3C788C"     ; 4 blue
                     "#6EA0B4"     ; 5 magenta
                     "#6E8CA0"    ; 6 cyan
                     "#96BEC8"      ; 7 white
                     "#506070"       ; 8 personal
                     "#706050")); 9 personal
(update-color-map (current-screen))
