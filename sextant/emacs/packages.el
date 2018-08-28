;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(;; File-specific modes
    adoc-mode
    d-mode
    gnuplot-mode
    graphviz-dot-mode
    haskell-mode
    markdown-mode
    rust-mode
    scss-mode
    sml-mode
    tuareg
    web-mode
    yaml-mode
    ;; Lisp
    sly
    ;; Minor modes and tools
    ag
    auto-complete
    company
    disable-mouse
    flycheck
    flyspell
    grizzl
    highlight-indentation
    ido
    paredit
    powerline
    projectile
    rainbow-delimiters
    rainbow-mode
    unicode-fonts
    ;; Themes
    sublime-themes
    ))

(defun packages-installed-p ()
  (not (memq 'nil (mapcar (lambda (p) (package-installed-p p)) my-packages))))

(unless (packages-installed-p)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
