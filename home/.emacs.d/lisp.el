;;; Common Lisp options

;(load (expand-file-name "~/quicklisp/log4slime-setup.el"))
;(global-log4slime-mode 1)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl --noinform")

(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-repl
               slime-asdf
               slime-fuzzy
               slime-banner
               slime-indentation
               slime-xref-browser
               slime-media))

(setq slime-enable-evaluate-in-emacs t)

(defun nm-slime-media-insert-image (image string)
  (with-current-buffer (slime-output-buffer)
    (let ((marker (slime-output-target-marker :repl-result)))
      (goto-char marker)
      (slime-propertize-region `(face slime-repl-result-face
                                      rear-nonsticky (face))
        (insert-image image string)
        (insert ?\n))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point)))
    (slime-repl-show-maximum-output)))
