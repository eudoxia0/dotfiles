;;;; Package specific options

;;; Org mode

(require 'ess-site)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (ditaa . t)))

(setq org-ditaa-jar-path "/usr/bin/ditaa") ; sigh

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-export-with-smart-quotes t)
;;; Modes

(setq auto-mode-alist
      (append '(("\\.md\\'" . markdown-mode)
		("\\.\\(yaml\\|yml\\)$" . yaml-mode)
		("\\.d$" . d-mode)
		("\\.clj$" . clojure-mode)
		("\\.textile$" . textile-mode)
		("\\.gp$" . gnuplot-mode)
		("\\.sql$" . sql-mode)
		("\\.\\(rb\\|Gemfile\\|Vagrantfile\\|Rakefile\\)$" . enh-ruby-mode)
		("\\.hs$" . haskell-mode)
                ("\\.dot$" . graphviz-dot-mode)
                ;; Web modes
                ("\\.html$" . web-mode)
                ("\\.tmpl$" . web-mode))
	    auto-mode-alist))

;; Autocomplete

(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; Projectile

(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;; Highlight indentation

(require 'highlight-indentation)
(add-hook 'prog-mode
           (lambda () (highlight-indentation-current-column-mode)))

;; Powerline

(require 'powerline)

;; Web-mode

(setq web-mode-engines-alist
'(("\\.html\\" . "django")
  ("\\.tmpl\\" . "closure")))

;; JSLint

(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'common-lisp-mode 'flymake-mode)

(setq pycodechecker "epylint")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

;; CEDET
