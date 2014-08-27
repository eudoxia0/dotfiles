(in-package :stumpwm)

;;;; Send fake clicks or strings

(defun send-click ()
  (run-shell-command "xdotool click 1"))

(defun send-string (str)
  (run-shell-command (format nil "xdotool type ~S" str)))

;;;; Input Presets
;;;; -------------
;;;;
;;;; An input preset is a key combination that sends a string to the active
;;;; window. Usually it's Unicode characters or character strings (e.g. Emoji
;;;; with Japanese characters), or long sequences of ASCII text like Latex
;;;; stuff.
;;;;
;;;; Key combination: Alt_R-[section] [keys]
;;;;
;;;; `section` is a key that corresponds to a group of input presets. For
;;;; example, to type the Greek letter lowercase Alpha, press `Alt_R+g a` ('g'
;;;; is the name of the Greek keyboard section, and the letter 'a' corresponds
;;;; to lowercase alpha).

(defmacro def-input-preset (section-name name key str)
  `(progn
     (defcommand ,name () ()
       (send-string ,str))
     (define-key *top-map*
                 (kbd ,(format nil "H-~A-~A" section-name key))
                 ,(symbol-name name))))

(defmacro define-input-presets (section-name &rest presets)
  `(progn
     ,@(mapcar #'(lambda (preset)
                   `(def-input-preset ,section-name ,@preset))
               presets)))

;;; Greek Alphabet (g)

(define-input-presets "g"
  (alpha   "a" "α")
  (beta    "b" "β")
  (gamma   "g" "γ")
  (delta   "d" "δ")
  (deltau  "D" "Δ")
  (epsilon "e" "ε")
  (zeta    "z" "ζ")
  (lambda  "l" "λ"))

;;; Latex Symbols

(define-input-presets "l"
  (in "i" "\in")
  (subset "c" "\subset")
  (nullset "0" "\emptyset"))

;;; Emoji (e)

(define-input-presets "e"
  (grinning-cat "c" "😸")
  (idunno       "?" "¯\_(ツ)_/¯"))

;;; Misc. Typographic Elements

(define-input-presets "."
  (ellipsis "..." "…")
  (ndash    "nd" "–")
  (mdash    "md" "— "))
