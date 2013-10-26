;;; Package specific options

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;;; Modes

(setq auto-mode-alist
      (append '(("\\.md\\'" . markdown-mode)
		("\\.\\(yaml\\|yml\\)$" . yaml-mode)
		("\\.d$" . d-mode)
		("\\.clj$" . clojure-mode)
		("\\.textile$" . textile-mode)
		("\\.gp$" . gnuplot-mode)
		("\\.sql$" . sql-mode)
		("\\.\\(rb\\|Gemfile\\|Vagrantfile\\|Rakefile\\)$" . ruby-mode)
		("\\.hs$" . haskell-mode))
	      auto-mode-alist))
