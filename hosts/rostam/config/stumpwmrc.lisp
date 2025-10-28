;;;; -*- mode: Lisp -*-
;;;;
;;;; stumpwm is cool as hell and if you disagree get the fuck out of here
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

;; Wipe the prefix map. Start from a clean slate.
(setf *root-map* (make-sparse-keymap))
(setf *tile-group-root-map* (make-sparse-keymap))
(setf *group-root-map* (make-sparse-keymap))
(setf *float-group-root-map* (make-sparse-keymap))

;; Utilities.
(defmacro defkey (k c)
  `(define-key *top-map* (kbd ,k) ,c))

(defmacro defprefixkey (k c)
  `(define-key *root-map* (kbd ,k) ,c))

;; Quit.
(defkey "s-Q" "quit")

;; Reload the configuration.
(defkey "s-R" "loadrc")

;; Switch workspaces.
(defkey "s-1" "gselect 1")
(defkey "s-2" "gselect 2")
(defkey "s-3" "gselect 3")
(defkey "s-4" "gselect 4")
(defkey "s-5" "gselect 5")

;; Move the current window to another workspace.
(defkey "s-!" "gmove 1")
(defkey "s-@" "gmove 2")
(defkey "s-#" "gmove 3")
(defkey "s-$" "gmove 4")
(defkey "s-%" "gmove 5")

;; Switch to the next window in the current frame.
(defkey "s-n" "pull-hidden-next")

;; Switch to the previous window in the current frame.
(defkey "s-p" "pull-hidden-previous")

;; Prompt for a shell command to run.
(defkey "s-r" "exec")

;; Close the current window.
(defkey "s-q" "delete-window")

;; Launch Firefox.
(defkey "C-s-w" "exec firefox")

;; Launch Emacs.
(defkey "C-s-e" "exec emacs")

;; Launch the file manager.
(defkey "C-s-f" "exec caja")

;; Launch the terminal.
(defkey "C-s-c" "exec alacritty")

;; Prompt for a stumpwm command to run.
(defprefixkey ";" "colon")

;; Prompt for a Lisp sexp to evaluate.
(defprefixkey ":" "eval")

;; Print the stumpwm version.
(defprefixkey "v" "version")

;; Split current frame vertically.
(defprefixkey "s" "vsplit")

;; Split current frame horizontally.
(defprefixkey "S" "hsplit")

;; Interactively resize the current frame.
(defprefixkey "r" "iresize")

;; Maximize the current frame to fill the screen.
(defprefixkey "Q" "only")

;; Rebalance frames.
(defprefixkey "=" "balance-frames")

;; Switch focus to the frame in the given direction.
(defprefixkey "Up" "move-focus up")
(defprefixkey "Down" "move-focus down")
(defprefixkey "Left" "move-focus left")
(defprefixkey "Right" "move-focus right")

;; Move the current window to the frame in the given direction.
(defprefixkey "M-Up" "move-window up")
(defprefixkey "M-Down" "move-window down")
(defprefixkey "M-Left" "move-window left")
(defprefixkey "M-Right" "move-window right")

;; Print the list of windows in this group.
(defprefixkey "w" "windows")

;; Toggle the modeline on/off.
(defprefixkey "m" "mode-line")

;; Lock the screen with xscreensaver.
(defprefixkey "x" "exec xscreensaver-command --lock")

(defparameter *config-map*
  (let ((m (make-sparse-keymap)))
    ;; Open Emacs to the stumpwmrc configuration.
    (define-key m (kbd "s") "exec emacs ~/dotfiles/hosts/rostam/config/stumpwmrc.lisp")
    ;; Open Emacs to the Nix configuration for rostam.
    (define-key m (kbd "n") "exec emacs ~/dotfiles/hosts/rostam/rostam.nix")
    ;; Open Emacs to the Emacs init.el.
    (define-key m (kbd "e") "exec emacs ~/dotfiles/hosts/rostam/config/init.el")
    m))

;; Prefix key of the config map.
(define-key *root-map* (kbd "c") '*config-map*)

;;;
;;; Useless Gaps
;;;
;;; Based on: https://github.com/stumpwm/stumpwm-contrib/tree/326725802fcedc2f8f28df91d4b548743da9e7bc/util/swm-gaps
;;;

;; Head gaps run along the 4 borders of the monitor(s).
(defparameter *head-gaps-size* 10)

;; Inner gaps run along all the 4 borders of a window.
(defparameter *inner-gaps-size* 10)

;; Outer gaps add more padding to the outermost borders of a window (touching the screen border).
(defparameter *outer-gaps-size* 0)

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

      ;; Only do width or height subtraction if result will be positive,
      ;; otherwise stumpwm will crash. Also, only modify window dimensions
      ;; if needed (i.e. window at least fills frame minus gap).
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

;;;
;;; Startup
;;;

(stumpwm::run-commands
 "grename 1"
 "gnew 2"
 "gnew 3"
 "gnew 4"
 "gnew 5"
 "gselect 1"
 "toggle-gaps-on")

(run-shell-command "polybar")
(run-shell-command "feh --bg-fill ~/.local/share/panther.jpg")
