(require 'eudoxia-package)

(require 'eudoxia-general)
(require 'eudoxia-lisp)
(require 'eudoxia-markdown)
(require 'eudoxia-mode-line)
(require 'eudoxia-olivetti)
(require 'eudoxia-splash)
(require 'eudoxia-ui)
(require 'eudoxia-xml)

;;;;
;;;; Custom Commands
;;;;

(defun e/persian ()
  "Use the Persian input method."
  (interactive)
  (set-input-method "farsi-transliterate-banan"))

(defvar special-inputs
  '(;; Punctuation
    ("--" . "–")
    ("---" . "—")
    ("..." . "…")
    ;; Spanish
    ("n~" . "ñ"))
  "Map of shorthand to special characters.")

(defun e/special ()
  "Enter special characters."
  (interactive)
  (let* ((completions (mapcar #'car special-inputs))
         (input (completing-read "e/special: " completions))n
         (output (cdr (assoc input special-inputs))))
    (if output
        (insert output)
        (message "Not a shorthand: %s" input))))

;;;;
;;;; Hydras
;;;;

(defhydra e/hydra-windows (:foreign-keys warn)
  "windows"
  ("x" delete-window)
  ("f" delete-other-windows)
  ("v" split-window-vertically)
  ("h" split-window-horizontally)
  ("o" other-window)
  (">" enlarge-window-horizontally)
  ("<" shrink-window-horizontally)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("r" (text-scale-increase 0))
  ("q" nil))

(global-set-key (kbd "C-v") 'e/hydra-windows/body)

(defhydra e/hydra-vi (:foreign-keys warn
                      :hint nil
                      :pre (set-cursor-color modal-cursor-color)
                      :post (set-cursor-color default-cursor-color))
  "modal editing"
  ("b" backward-char)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)

  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("N" (next-line 10))
  ("P" (previous-line 10))
  ;; Page Up
  ("<prior>" scroll-down-command)
  ;; Page Down
  ("<next>" scroll-up-command)

  ("w" kill-region)
  ("W" kill-ring-save)
  ("y" yank)

  ("s" isearch-forward)
  ("r" isearch-backward)
  ;("t" iy-go-to-char)
  ;("T" iy-go-to-char-backward)

  ("SPC" set-mark-command)
  ("G" goto-line)

  ("0" (digit-argument 0))
  ("1" (digit-argument 1))
  ("2" (digit-argument 2))
  ("3" (digit-argument 3))
  ("4" (digit-argument 4))
  ("5" (digit-argument 5))
  ("6" (digit-argument 6))
  ("7" (digit-argument 7))
  ("8" (digit-argument 8))
  ("9" (digit-argument 9))

  ("d" delete-char)
  ("D" kill-word)

  (":" execute-extended-command)
  ("x" replace-string)
  ("X" vr/query-replace)

  ("/" undo)

  ("B" switch-to-buffer)
  (">" next-buffer)
  ("<" previous-buffer)

  ("i" nil :color blue))

(global-set-key (kbd "<escape>") 'e/hydra-vi/body)

(provide 'eudoxia)
