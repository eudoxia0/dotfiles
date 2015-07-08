;;; Variables

(defgroup scriba nil
  "Scriba Mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'docs)

(defcustom scriba-mode-hook nil
  "List of functions to call when entering Scriba mode."
  :type 'hook
  :options '(flyspell-mode))

;;; Syntax highlighting

(defvar scriba-font-lock-keywords
  '(("@[a-zA-Z]+" . font-lock-keyword-face)
    ("@begin(\\([a-zA-Z]+\\))" 1 font-lock-function-name-face)
    ("@end(\\([a-zA-Z]+\\))" 1 font-lock-function-name-face)))

;;; Text insertion and manipulation

(defun scriba-insert-at-sign ()
  (interactive)
  (insert "@"))

(defun scriba-insert-block (tag-name)
  "Insert a @begin/@end block."
  (interactive (list (read-string "Tag: ")))
  (insert "@begin(" tag-name ")\n\n@end(" tag-name ")"))

(defun scriba-insert-section (title)
  "Insert a section."
  (interactive (list (read-string "Title: ")))
  (insert "@begin(section)\n@title(" title ")\n\n@end(section)"))

(defun scriba-wrap-text (before-text after-text)
  "Wrap the selected region in text."
  (let ((begin (mark))
        (end (point)))
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (insert before-text)
      (goto-char (point-max))
      (insert after-text))))

(defun scriba-apply-markup (tag)
  "Wrap the selected region in markup."
  (scriba-wrap-text (concat "@" tag "(")
                    ")"))

(defun scriba-boldify ()
  (interactive)
  (scriba-apply-markup "b"))

(defun scriba-italicize ()
  (interactive)
  (scriba-apply-markup "i"))

(defun scriba-underline ()
  (interactive)
  (scriba-apply-markup "u"))

(defun scriba-strikethrough ()
  (interactive)
  (scriba-apply-markup "strike"))

(defun scriba-codify ()
  (interactive)
  (scriba-apply-markup "c"))

(defun scriba-superscript ()
  (interactive)
  (scriba-apply-markup "sup"))

(defun scriba-subscript ()
  (interactive)
  (scriba-apply-markup "sub"))

;;; Keybindings

(defvar scriba-mode-map
  (let ((map (make-keymap)))
    ;; Inserting strings
    (define-key map "\t"        'scriba-insert-at-sign)
    (define-key map "\C-c\C-cb" 'scriba-insert-block)
    (define-key map "\C-c\C-cs" 'scriba-insert-section)

    ;; Markup
    (define-key map "\C-c\C-sb" 'scriba-boldify)
    (define-key map "\C-c\C-si" 'scriba-italicize)
    (define-key map "\C-c\C-su" 'scriba-underline)
    (define-key map "\C-c\C-ss" 'scriba-strikethrough)
    (define-key map "\C-c\C-sc" 'scriba-codify)
    (define-key map "\C-c\C-s^" 'scriba-superscript)
    (define-key map "\C-c\C-sv" 'scriba-subscript)
    map))

;;; Menu

(easy-menu-define scriba-mode-menu
  scriba-mode-map
  "Menu used for `scriba-mode'."
  '("Scriba"
    ["Insert block"   scriba-insert-block   t]
    ["Insert section" scriba-insert-section t]
    "----"
    ["Bold"           scriba-boldify        t]
    ["Italic"         scriba-italicize      t]
    ["Underline"      scriba-underline      t]
    ["Strikethrough"  scriba-strikethrough  t]
    ["Code"           scriba-codify         t]
    ["Superscript"    scriba-superscript    t]
    ["Subscript"      scriba-subscript      t]))

;;; File extensions

(add-to-list 'auto-mode-alist
             '("\\.scr\\'" . scriba-mode))

;;; Mode

(define-derived-mode scriba-mode text-mode "Scriba"
  "Major mode for editing Scriba files."
  (setq font-lock-defaults '((scriba-font-lock-keywords))))

(provide 'scriba)
