;;;;
;;;; stumpwm is cool as hell and if you disagree get the fuck out of
;;;; here
;;;;
(in-package :stumpwm)

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

(defmacro defprefixkey (k c)
  `(define-key *root-map* (kbd ,k) ,c))

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
(defkey "s-r" "exec")

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

;; Toggle the modeline on/off.
(defprefixkey "m" "mode-line")

;; Lock the screen with xscreensaver.
(defprefixkey "x" "exec xscreensaver-command --lock")
(defkey "Pause" "exec xscreensaver-command --lock")

;;;
;;; Startup
;;;

;; Start polybar.
(run-shell-command "polybar 2> ~/.polybar.log")

;; Set the wallpaper.
(run-shell-command "feh --no-fehbg --bg-fill ~/.eudoxia.d/data/wallpaper/panther.jpg")

;; Better default pointer style.
(run-shell-command "xsetroot -cursor_name left_ptr")

;; On startup, restart services that are killed by quitting the window manager.
(run-shell-command "systemctl --user start network-manager-applet")
(run-shell-command "systemctl --user start xcape")
(run-shell-command "systemctl --user start xdg-desktop-portal-gtk")
(run-shell-command "systemctl --user start xscreensaver")
