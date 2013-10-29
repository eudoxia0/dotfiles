;;;; -*- Lisp -*-
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Because I'm tired of symlinking everything to local-projects/ and because
;;; local-projects sometimes breaks for some reason ¯\_(ツ)_/¯

(in-package #:asdf)
(defvar *subdir-search-registry* (list (merge-pathnames "code/"
                                                        (user-homedir-pathname))
                                       #p"/mnt/d/code/")
  "List of directories to search subdirectories within.")
(defvar *subdir-search-wildcard* :wild
  "Value of :wild means search only one level of subdirectories; value of :wild-inferiors means search
 all levels of subdirectories (I don't advise using this in big directories!)")
(defun sysdef-subdir-search (system)
  (let ((latter-path (make-pathname :name (coerce-name system)
                                    :directory (list :relative
                                                     *subdir-search-wildcard*)
                                    :type "asd"
                                    :version :newest
                                    :case :local)))
    (dolist (d *subdir-search-registry*)
      (let* ((wild-path (merge-pathnames latter-path d))
             (files (directory wild-path)))
        (when files
          (return (first files)))))))
(pushnew 'sysdef-subdir-search *system-definition-search-functions*)
(in-package :cl-user)

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))

;;; Safety first!
(declaim (optimize (safety 3) (debug 3) (speed 0)))
