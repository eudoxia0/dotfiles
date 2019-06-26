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

;;; Useless gaps

(defparameter *inner-gaps-size* 30)
(defparameter *outer-gaps-size* 45)
(defparameter *gaps-on* nil)

;; Redefined - with `if`s for *inner-gaps-on*
(defun stumpwm::maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y wx wy width height border stick)
      (stumpwm::geometry-hints win)

    (if (and *gaps-on* (not (stumpwm::window-transient-p win)))
        (setf width (- width (* 2 *inner-gaps-size*))
              height (- height (* 2 *inner-gaps-size*))
              x (+ x *inner-gaps-size*)
              y (+ y *inner-gaps-size*)))

    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
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
                                                               (if (and *gaps-on* (not (stumpwm::window-transient-p win))) (* 2 *inner-gaps-size*) 0))
                  (xlib:drawable-height (window-parent win)) (- (stumpwm::frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                (if (and *gaps-on* (not (stumpwm::window-transient-p win))) (* 2 *inner-gaps-size*) 0)))))
      ;; update the "extents"
      (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                            (list wx wy
                                  (- (xlib:drawable-width (window-parent win)) width wx)
                                  (- (xlib:drawable-height (window-parent win)) height wy))
                            :cardinal 32))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (let ((windows (mapcan (lambda (g)
                           (mapcar (lambda (w) w) (stumpwm::sort-windows g)))
                         (stumpwm::sort-groups (current-screen)))))
    (mapcar (lambda (w)
              (if (string= (class-name (class-of w)) "TILE-WINDOW")
                  (stumpwm::maximize-window w))) windows)))

;; Redefined neighbour for working with outer gaps
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
            ;; Two edges are neighbours if they have the same offset and their starts and ends
            ;; overlap.  We want to find the neighbour that overlaps the most.
            (when (and (= (abs (- src-offset offset)) nearest-edge-diff)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun add-outer-gaps ()
  "Add extra gap to the outermost borders"
  (mapcar (lambda (head)
            (let* ((height (stumpwm::head-height head))
                   (width (stumpwm::head-width head))
                   (x (stumpwm::head-x head))
                   (y (stumpwm::head-y head))
                   (gap *outer-gaps-size*)
                   (new-height (- height (* 2 gap)))
                   (new-width (- width (* 2 gap))))
              (stumpwm::resize-head
               (stumpwm::head-number head)
               (+ x gap) (+ y gap)
               new-width new-height)))
          (screen-heads (current-screen))))

(defcommand toggle-gaps () ()
  "Toggle gaps"
  (setf *gaps-on* (null *gaps-on*))
  (if *gaps-on*
      (progn
        (add-outer-gaps)
        (reset-all-windows))
      (stumpwm::refresh-heads)))

(stumpwm::run-commands "toggle-gaps")
