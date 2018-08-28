;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(;; Major modes
    sly yaml-mode clojure-mode d-mode textile-mode markdown-mode
    gnuplot-mode erlang haskell-mode fsharp-mode scss-mode
    rainbow-mode enh-ruby-mode web-mode projectile grizzl
    graphviz-dot-mode powerline rust-mode tuareg adoc-mode ;pov-mode idris-mode
    sml-mode disable-mouse
    ;; Other plugins
    rainbow-delimiters ecb auto-complete ag highlight-indentation
    flycheck ido flyspell paredit company unicode-fonts
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
