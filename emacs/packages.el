;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(org nav
    ;; Major modes
    sly yaml-mode clojure-mode d-mode textile-mode markdown-mode
    gnuplot-mode erlang haskell-mode fsharp-mode sass-mode
    rainbow-mode enh-ruby-mode web-mode projectile grizzl
    graphviz-dot-mode ess cmake-mode mediawiki scala-mode2 ensime
    powerline rust-mode tuareg adoc-mode ;pov-mode idris-mode
    jsx-mode
    ;; disabled: rw-hunspell pep8
    ;; Other plugins
    rainbow-delimiters magit ecb auto-complete ag highlight-indentation
    google-c-style flycheck ido flyspell gnugo paredit company cider
    ;; Themes
    ;qsimpleq-theme
    ;color-theme-solarized color-theme-twilight color-theme-github
    ;soft-morning-theme zenburn-theme qsimpleq-theme noctilux-theme
    sublime-themes ;espresso-theme tronesque-theme naquadah-theme
    ))

(defun packages-installed-p ()
  (not (memq 'nil (mapcar (lambda (p) (package-installed-p p)) my-packages))))

(unless (packages-installed-p)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
