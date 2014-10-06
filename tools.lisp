(ql:quickload (list :drakma :cl-json :trivial-utf-8))

;;; JSON requests

(defun json-req (&rest params)
  (cl-json:decode-json-from-string
   (trivial-utf-8:utf-8-bytes-to-string
    (apply #'drakma:http-request params))))

;;; Markdown documentation generation

(defun document-function (fn)
  (format nil "* `~A ~A`: ~A"
          (swank-backend:function-name fn)
          (swank-backend:arglist fn)
          (documentation fn t)))

(defun reintern (symbol package)
  (intern (symbol-name symbol) (find-package package)))

(defmacro docgen (package &rest body)
  `(format t "~{~A~%~%~}"
           (list ,@(loop for elem in body collecting
                     (if (stringp elem)
                         elem
                         `(document-function #',(reintern elem package)))))))
