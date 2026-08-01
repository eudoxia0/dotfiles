;;;;
;;;; stumpwm is cool as hell and if you disagree get the fuck out of
;;;; here
;;;;
(in-package :stumpwm)

;;;
;;; Modules
;;;

(load "~/.stumpwm.d/gaps.lisp")

;;;
;;; Appearance
;;;

;; Font.
(set-font "-xos4-terminus-medium-r-normal-*-28-*-*-*-*-*-*-*")

;;;
;;; General Configuration
;;;

(setf *mouse-focus-policy* :sloppy)

;;;
;;; Keybindings
;;;

;; Prefix key: logo+w.
(set-prefix-key (kbd "s-w"))

;; Utilities.
(defmacro defkey (k c)
  `(define-key *top-map* (kbd ,k) ,c))

;; Quit.
(defkey "s-Q" "quit")

;; Reload the configuration.
(defkey "s-R" "loadrc")

;; Define a new set of workspaces.

(stumpwm::run-commands
 "grename α"
 "gnew β"
 "gnew γ"
 "gnew δ"
 "gnew ε"
 "gselect α")

;; Switch workspaces.
(defkey "s-1" "gselect α")
(defkey "s-2" "gselect β")
(defkey "s-3" "gselect γ")
(defkey "s-4" "gselect δ")
(defkey "s-5" "gselect ε")

;; Move the current window to another workspace.
(defkey "s-!" "gmove α")
(defkey "s-@" "gmove β")
(defkey "s-#" "gmove γ")
(defkey "s-$" "gmove δ")
(defkey "s-%" "gmove ε")

;; Prompt for a shell command to run.
(defkey "s-r" "exec rofi -show run")

;; Close the current window.
(defkey "s-q" "delete-window")

;; Launch Firefox.
(defkey "C-s-w" "exec firefox")

;; Launch Emacs.
(defkey "C-s-e" "exec emacs")

;; Launch the file manager.
(defkey "C-s-f" "exec thunar")

;; Launch the terminal.
(defkey "C-s-c" "exec alacritty")

;; Take a screenshot of a selected region.
(defkey "Print" "exec scrot -f -s")

;; Lock the screen with xscreensaver.
(defkey "Pause" "exec xscreensaver-command --lock")

;; Toggle the modeline on/off.
(define-key *root-map* (kbd "m") "mode-line")

;; Toggle gaps on/off.
(define-key *root-map* (kbd "g") "toggle-gaps")

;; Focus on the next window in this frame.
(define-key *top-map* (kbd "s-TAB") "next-in-frame")

;; Remove default keybindings I don't use.
(dolist (key (list "c"
                   "C-c"
                   "e"
                   "C-e"
                   "C-a"
                   "!"
                   "w"
                   "x"
                   "F1"
                   "F2"
                   "F3"
                   "F4"
                   "F5"
                   "F6"
                   "F7"
                   "F8"
                   "F9"
                   "F10"
                   "b"
                   "C-b"))
  (undefine-key *root-map* (kbd key)))

(dolist (key (list "#"
                   "RET"
                   "C-RET"
                   "DEL"
                   "k"
                   "C-k"
                   "K"
                   "C-u"
                   "M-n"
                   "M-p"
                   "C-N"
                   ">"
                   "0"
                   "1"
                   "2"
                   "3"
                   "4"
                   "5"
                   "6"
                   "7"
                   "8"
                   "9"))
  (undefine-key *group-root-map* (kbd key)))

(dolist (key (list "C-0"
                   "C-1"
                   "C-2"
                   "C-3"
                   "C-4"
                   "C-5"
                   "C-6"
                   "C-7"
                   "C-8"
                   "C-9"
                   "l"
                   "C-l"
                   "F"
                   "f"
                   "TAB"
                   "M-TAB"
                   "n"
                   "C-n"
                   "C-M-n"
                   "SPC"
                   "C-SPC"
                   "p"
                   "C-p"
                   "C-M-p"
                   "q"
                   "P"
                   "W"
                   "M-t"))
  (undefine-key *tile-group-root-map* (kbd key)))

;;;
;;; Mode Line
;;;

;; Put the mode line in the bottom.
(setf *mode-line-position* :top)

;; Enable mode line.
(toggle-mode-line (current-screen) (current-head))

;;;
;;; Startup
;;;

;; Start polybar.
;(run-shell-command "polybar 2> ~/.polybar.log")

;; Set the wallpaper.
(run-shell-command "feh --no-fehbg --bg-fill ~/.eudoxia.d/data/wallpaper/panther.jpg")

;; Better default pointer style.
(run-shell-command "xsetroot -cursor_name left_ptr")

;; On startup, restart services that are killed by quitting the window manager.
(run-shell-command "systemctl --user start network-manager-applet")
(run-shell-command "systemctl --user start xcape")
(run-shell-command "systemctl --user start xdg-desktop-portal-gtk")
(run-shell-command "systemctl --user start xscreensaver")
