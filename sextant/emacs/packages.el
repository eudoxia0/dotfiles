;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(;; File-specific modes
    yaml-mode
    d-mode
    markdown-mode
    gnuplot-mode
    haskell-mode
    scss-mode
    rainbow-mode
    web-mode
    projectile
    grizzl
    graphviz-dot-mode
    powerline
    rust-mode
    tuareg
    adoc-mode
    sml-mode
    ;; Lisp
    sly
    ;; Minor modes and tools
    rainbow-delimiters
    auto-complete
    ag
    highlight-indentation
    flycheck
    ido
    flyspell
    paredit
    company
    unicode-fonts
    disable-mouse
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
