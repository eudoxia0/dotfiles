(ql:quickload (list :drakma :cl-json :trivial-utf-8))

(defun json-req (&rest params)
  (cl-json:decode-json-from-string
   (trivial-utf-8:utf-8-bytes-to-string
    (apply #'drakma:http-request params))))

