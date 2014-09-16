(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (ditaa . t)))

(setq org-ditaa-jar-path "/usr/bin/ditaa") ; sigh

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-export-with-smart-quotes t)
