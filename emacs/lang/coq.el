;;;; Coq

;(load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")

;(require 'unicode-tokens)

(add-hook 'coq-mode-hook
          (lambda ()
            (proof-unicode-tokens-enable)))
