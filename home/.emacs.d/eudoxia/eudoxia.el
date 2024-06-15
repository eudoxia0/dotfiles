(require 'eudoxia-package)

(require 'eudoxia-general)
(require 'eudoxia-lisp)
(require 'eudoxia-markdown)
(require 'eudoxia-mode-line)
(require 'eudoxia-olivetti)
(require 'eudoxia-persian)
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

(provide 'eudoxia)
