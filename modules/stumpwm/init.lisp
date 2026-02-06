;;;;
;;;; stumpwm is cool as hell and if you disagree get the fuck out of
;;;; here
;;;;
(in-package :stumpwm)

;; Prefix key: logo+w.
(set-prefix-key (kbd "s-w"))

;; Utilities.
(defmacro defkey (k c)
  `(define-key *top-map* (kbd ,k) ,c))

(defmacro defprefixkey (k c)
  `(define-key *root-map* (kbd ,k) ,c))

;;;
;;; Modules
;;;

(load "~/.stumpwm.d/gaps.lisp")
(load "~/.stumpwm.d/bright.lisp")

;;;
;;; General Configuration
;;;

(setf *mouse-focus-policy* :sloppy)

;;;
;;; Custom Commands
;;;

(defun toggle-mode-line* ()
  (toggle-mode-line (current-screen) (current-head)))

(defcommand polybar-restart () ()
  "Restart Polybar."
  (toggle-mode-line*)
  (run-shell-command "polybar-msg cmd restart")
  (toggle-mode-line*))

;;;
;;; Keybindings
;;;

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
(defkey "C-s-f" "exec thunar /home/eudoxia/root")

;; Launch the terminal.
(defkey "C-s-c" "exec alacritty")

;; Take a screenshot of the whole screen.
(defkey "Print" "exec scrot")

;; Take a screenshot of a selected region.
(defkey "C-Print" "exec scrot -f -s")

;; Toggle the modeline on/off.
(defprefixkey "m" "mode-line")

;; Lock the screen with xscreensaver.
(defprefixkey "x" "exec xscreensaver-command --lock")
(defkey "Pause" "exec xscreensaver-command --lock")

;; Toggle gaps on/off.
(defprefixkey "g" "toggle-gaps")

;;;
;;; Volume Control (PipeWire)
;;;

(defcommand volume-up () ()
  "Increase volume."
  (run-shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"))

(defcommand volume-down () ()
  "Decrease volume."
  (run-shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"))

(defcommand volume-mute () ()
  "Toggle mute."
  (run-shell-command "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

;; Media key bindings for volume control.
(defkey "XF86AudioRaiseVolume" "volume-up")
(defkey "XF86AudioLowerVolume" "volume-down")
(defkey "XF86AudioMute" "volume-mute")

;;;
;;; Host-Specific Config
;;;

(defun rostamp ()
  (string= (uiop:hostname) "rostam"))

(defun ismenep ()
  (string= (uiop:hostname) "ismene"))

;;;
;;; Startup
;;;

; We only want gaps in rostam, since ismene is too small.
(if (rostamp)
    (stumpwm::run-commands "toggle-gaps-on"))

; Laptop brightness control keys for ismene only.
(when (ismenep)
  (defkey "XF86MonBrightnessUp" "laptop-brightness-up")
  (defkey "XF86MonBrightnessDown" "laptop-brightness-down"))

;; Ensure xscreensaver is started.
(run-shell-command "systemctl --user start xscreensaver")

;; Start polybar.
(run-shell-command "polybar 2> ~/.polybar.log")

;; Set the wallpaper.
(run-shell-command "feh --no-fehbg --bg-fill ~/.eudoxia.d/data/wallpaper/semiramis.jpg")

;; Better default pointer style.
(run-shell-command "xsetroot -cursor_name left_ptr")

;; On startup, restart services that are killed by quitting the window manager.
(run-shell-command "systemctl --user start nm-applet")
(run-shell-command "systemctl --user start xcape")
(run-shell-command "systemctl --user start xdg-desktop-portal-gtk")

(if (rostamp)
    (progn
      (setf (getenv "GDK_DPI_SCALE") "1.5")
      (setf (getenv "QT_SCALE_FACTOR") "1.5")))
