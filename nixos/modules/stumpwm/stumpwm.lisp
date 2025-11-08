;;;; -*- mode: Lisp -*-
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

;; Take a screenshot of the whole screen.
(defkey "Print" "exec scrot-screenshot")

;; Take a screenshot of a selected region.
(defkey "C-Print" "exec scrot-screenshot -f -s")

;; Toggle the modeline on/off.
(defprefixkey "m" "mode-line")

;; Lock the screen with xscreensaver.
(defprefixkey "x" "exec xscreensaver-command --lock")
(defkey "Pause" "exec xscreensaver-command --lock")

(defparameter *config-map*
  (let ((m (make-sparse-keymap)))
    ;; Open Emacs to the stumpwmrc configuration.
    (define-key m (kbd "s") "exec emacs ~/dotfiles/hosts/rostam/mod/stumpwm/stumpwm.lisp")
    ;; Open Emacs to the Nix configuration for rostam.
    (define-key m (kbd "n") "exec emacs ~/dotfiles/hosts/rostam/rostam.nix")
    ;; Open Emacs to the Emacs init.el.
    (define-key m (kbd "e") "exec emacs ~/dotfiles/hosts/rostam/mod/emacs/init.el")
    m))

;; Prefix key of the config map.
(define-key *root-map* (kbd "c") '*config-map*)

;;;
;;; Useless Gaps
;;;
;;; Based on: https://github.com/stumpwm/stumpwm-contrib/tree/326725802fcedc2f8f28df91d4b548743da9e7bc/util/swm-gaps
;;;

;; Head gaps run along the 4 borders of the monitor(s). NOTE: if you
;; have a systray (e.g. polybar) a positive value of this parameter
;; will clip the sides of it.
(defparameter *head-gaps-size* 0)

;; Inner gaps run along all the 4 borders of a window.
(defparameter *inner-gaps-size* 10)

;; Outer gaps add more padding to the outermost borders of a window
;; (touching the screen border).
(defparameter *outer-gaps-size* 10)

;; Whether or not gaps are on.
(defparameter *gaps-on* nil)

(defun apply-gaps-p (win)
  "Tell if gaps should be applied to this window"
  (and *gaps-on* (not (stumpwm::window-transient-p win)) (not (window-fullscreen win))))

(defun window-edging-p (win direction)
  "Tell if the window is touching the head in the given direction."
  (let* ((frame (stumpwm::window-frame win))
         (head (stumpwm::frame-head (stumpwm:window-group win) frame))
         (offset (nth-value 2 (stumpwm::get-edge frame direction))))
    (ecase direction
      (:top
       (= offset (stumpwm::head-y head)))
      (:bottom
       (= offset (+ (stumpwm::head-y head) (stumpwm::head-height head))))
      (:left
       (= offset (stumpwm::head-x head)))
      (:right
       (= offset (+ (stumpwm::head-x head) (stumpwm::head-width head)))))))

(defun gaps-offsets (win)
  "Return gap offset values for the window. X and Y values are added. WIDTH and
HEIGHT are subtracted."
  (let ((x *inner-gaps-size*)
        (y *inner-gaps-size*)
        (width (* 2 *inner-gaps-size*))
        (height (* 2 *inner-gaps-size*)))
    (if (window-edging-p win :top)
        (setf y (+ y *outer-gaps-size*)
              height (+ height *outer-gaps-size*)))
    (if (window-edging-p win :bottom)
        (setf height (+ height *outer-gaps-size*)))
    (if (window-edging-p win :left)
        (setf x (+ x *outer-gaps-size*)
              width (+ width *outer-gaps-size*)))
    (if (window-edging-p win :right)
        (setf width (+ width *outer-gaps-size*)))
    (values x y width height)))

(defun stumpwm::maximize-window (win)
  "Redefined gaps aware maximize function."
  (multiple-value-bind (x y wx wy width height border stick)
      (stumpwm::geometry-hints win)

    (let ((ox 0) (oy 0) (ow 0) (oh 0)
          (frame (stumpwm::window-frame win)))
      (if (apply-gaps-p win)
          (multiple-value-setq (ox oy ow oh) (gaps-offsets win)))

      ;; Only do width or height subtraction if result will be
      ;; positive, otherwise stumpwm will crash. Also, only modify
      ;; window dimensions if needed (i.e. window at least fills frame
      ;; minus gap).
      (when (and (< ow width)
                 (>= width (- (frame-width frame) ow)))
        (setf width (- width ow)))
      (when (and (< oh height)
                 (>= height (- (stumpwm::frame-display-height (window-group win) frame) oh)))
        (setf height (- height oh)))

      (setf x (+ x ox)
            y (+ y oy))

      ;; This is the only place a window's geometry should change
      (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
      (xlib:with-state ((window-parent win))
        ;; FIXME: updating the border doesn't need to be run everytime
        ;; the window is maximized, but only when the border style or
        ;; window type changes. The overhead is probably minimal,
        ;; though.
        (setf (xlib:drawable-x (window-parent win)) x
              (xlib:drawable-y (window-parent win)) y
              (xlib:drawable-border-width (window-parent win)) border)
        ;; the parent window should stick to the size of the window
        ;; unless it isn't being maximized to fill the frame.
        (if (or stick
                (find *window-border-style* '(:tight :none)))
            (setf (xlib:drawable-width (window-parent win)) (window-width win)
                  (xlib:drawable-height (window-parent win)) (window-height win))
            (let ((frame (stumpwm::window-frame win)))
              (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                                 (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                 ow)
                    (xlib:drawable-height (window-parent win)) (- (stumpwm::frame-display-height (window-group win) frame)
                                                                  (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                  oh))))
        ;; update the "extents"
        (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                              (list wx
                                    (- (xlib:drawable-width (window-parent win)) width wx)
                                    wy
                                    (- (xlib:drawable-height (window-parent win)) height wy))
                              :cardinal 32))
      (update-configuration win))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (mapcar #'stumpwm::maximize-window
          (stumpwm::only-tile-windows (screen-windows (current-screen)))))

;; Redefined neighbour for working with head gaps
(defun stumpwm::neighbour (direction frame frameset)
  "Returns the best neighbour of FRAME in FRAMESET on the DIRECTION edge.
   Valid directions are :UP, :DOWN, :LEFT, :RIGHT.
   eg: (NEIGHBOUR :UP F FS) finds the frame in FS that is the 'best'
   neighbour above F."
  (let ((src-edge (ecase direction
                    (:up :top)
                    (:down :bottom)
                    (:left :left)
                    (:right :right)))
        (opposite (ecase direction
                    (:up :bottom)
                    (:down :top)
                    (:left :right)
                    (:right :left)))
        (best-frame nil)
        (best-overlap 0)
        (nearest-edge-diff nil))
    (multiple-value-bind (src-s src-e src-offset)
        (stumpwm::get-edge frame src-edge)

      ;; Get the edge distance closest in the required direction
      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (stumpwm::get-edge f opposite)
          (let ((offset-diff (abs (- src-offset offset))))
            (if nearest-edge-diff
                (if (< offset-diff nearest-edge-diff)
                    (setf nearest-edge-diff offset-diff))
                (setf nearest-edge-diff offset-diff)))))

      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (stumpwm::get-edge f opposite)
          (let ((overlap (- (min src-e e)
                            (max src-s s))))
            ;; Two edges are neighbours if they have the same offset (after
            ;; accounting for gaps) and their starts and ends overlap. We want
            ;; to find the neighbour that overlaps the most.
            (when (and (= (abs (- src-offset offset)) nearest-edge-diff)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun add-head-gaps ()
  "Add extra gap to the head boundary"
  (mapcar (lambda (head)
            (let* ((height (stumpwm::head-height head))
                   (width (stumpwm::head-width head))
                   (x (stumpwm::head-x head))
                   (y (stumpwm::head-y head))
                   (gap *head-gaps-size*)
                   (new-height (- height (* 2 gap)))
                   (new-width (- width (* 2 gap))))
              (stumpwm::resize-head
               (stumpwm::head-number head)
               (+ x gap) (+ y gap)
               new-width new-height)))
          (screen-heads (current-screen))))

(defcommand toggle-gaps () ()
  "Toggle gaps"
  (if (null *gaps-on*)
      (toggle-gaps-on)
      (toggle-gaps-off)))

(defcommand toggle-gaps-on () ()
  "Turn gaps on"
  (setf *gaps-on* t)
  (progn
    (add-head-gaps)
    (reset-all-windows)))

(defcommand toggle-gaps-off () ()
  "Turn gaps off"
  (setf *gaps-on* nil)
  (refresh-heads))

;; Toggle gaps on/off.
(defprefixkey "g" "toggle-gaps")

;;;
;;; Brightness Control
;;;

(defun brightness-get ()
  "Get the current brightness value from the external monitor using ddcutil."
  (let* ((output (run-shell-command "ddcutil getvcp 10" t))
         ;; Parse output like `VCP code 0x10 (Brightness): current value = 50, max value = 100`
         (current-pos (search "current value =" output)))
    (if current-pos
        (let ((start (+ current-pos (length "current value ="))))
          (multiple-value-bind (value end)
            (parse-integer output :start start :junk-allowed t)
            (or value 50)))
        ; Default to 50 if parsing fails
      50)))

(defun brightness-set (value)
  (let ((clamped-value (max 0 (min 100 value))))
    (run-shell-command (format nil "ddcutil setvcp 10 ~d" clamped-value))
    clamped-value))

(defun brightness-change (d)
  (let ((b (brightness-get)))
    (let ((b* (+ b d)))
      (brightness-set b*)
      (message "Brightness: ~d" b*))))

(defcommand brightness-up () ()
  "Increase brightness."
  (brightness-change 5))

(defcommand brightness-down () ()
  "Decrease brightness."
  (brightness-change -5))

(define-interactive-keymap ibrightness ()
  ((kbd "=") "brightness-up")
  ((kbd "-") "brightness-down"))

;; Interactively adjust brightness.
(defprefixkey "b" "ibrightness")

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
  (string= (uiop:hostname) "rostam"))

;;;
;;; Startup
;;;

; We only want gaps in rostam, since ismene is too small.
(if (rostamp)
    (stumpwm::run-commands "toggle-gaps-on"))

;; Ensure xscreensaver is started.
(run-shell-command "systemctl --user start xscreensaver")

;; Start polybar.
(run-shell-command "polybar 2> ~/.polybar.log")

;; Something something SSH.
(run-shell-command "dbus-update-activation-environment --systemd DISPLAY XAUTHORITY")

;; Set the wallpaper.
(run-shell-command "feh --no-fehbg --bg-fill ~/.local/share/eudoxia/panther.jpg")
