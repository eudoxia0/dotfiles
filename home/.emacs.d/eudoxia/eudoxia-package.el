;;;; Configure `package.el`, ensure required packages are installed.

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar eudoxia-package-list
  '(hydra
    olivetti
    aircon-theme
    markdown-mode
    treemacs))

(dolist (package eudoxia-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'eudoxia-package)
