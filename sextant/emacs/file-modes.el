;;;; Map filetypes and special filenames to modes

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
                ("\\.doc$" . adoc-mode)
                ;; Web modes
                ("\\.html$" . web-mode)
                ("\\.tmpl$" . web-mode)
                ("\\.eco$" . web-mode)
                ("\\.jsx$" . jsx-mode)
                ("\\.scss$" . scss-mode)
                ("\\.lass$" . lass-mode)
                ("\\.ctex$" . latex-mode))
	    auto-mode-alist))
