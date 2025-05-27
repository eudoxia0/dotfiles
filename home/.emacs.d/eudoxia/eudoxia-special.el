(defvar special-inputs
  '(;; Punctuation
    ("--" . "–")
    ("---" . "—")
    ("..." . "…"))
  "Map of shorthand to special characters.")

(defun eudoxia-special ()
  "Enter special characters."
  (interactive)
  (let* ((completions (mapcar #'car special-inputs))
         (input (completing-read "e/special: " completions))n
         (output (cdr (assoc input special-inputs))))
    (if output
        (insert output)
        (message "Not a shorthand: %s" input))))

(provide 'eudoxia-special)
