(in-package :stumpwm)

;;;; Keybindings

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

(undefine-key *root-map* (kbd "e")) ; Emacs
(undefine-key *root-map* (kbd "k")) ; Kill
(undefine-key *root-map* (kbd "c")) ; Console
(undefine-key *root-map* (kbd "!")) ; Run shell command

; Repeat bindings
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-a"))

;;; Workspaces

(define-key *top-map* (kbd "s-1") "gselect web")
(define-key *top-map* (kbd "s-2") "gselect term")
(define-key *top-map* (kbd "s-3") "gselect edit")
(define-key *top-map* (kbd "s-4") "gselect im")
(define-key *top-map* (kbd "s-5") "gselect file")
(define-key *top-map* (kbd "s-6") "gselect mail")

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
(define-key *top-map* (kbd "s-c") "terminal")
(define-key *top-map* (kbd "s-r") "exec")

; Take a screenshot
(define-key *root-map* (kbd "Print") "exec scrot")
