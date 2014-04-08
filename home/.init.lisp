;;;; -*- Lisp -*-

(setf *print-case* :downcase)

;;; The following lines added by ql:add-to-init-file:

#-quicklisp
(load (merge-pathnames ".cim/quicklisp/setup.lisp"
                       (user-homedir-pathname)))

;;; Safety first!
(declaim (optimize (safety 3) (debug 3) (speed 0)))

(sb-ext:restrict-compiler-policy 'debug 3)
