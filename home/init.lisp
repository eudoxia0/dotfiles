;;;; -*- Lisp -*-
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Better printing for hash tables
(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))

;;; Better printing for objects

#|
(require :mop-utils)

(defun class-slots (obj)
  (mop-utils:slot-names-of obj))

(defun slot-values (obj)
  (loop for slot in (class-slots obj)
        collecting
        (list (symbol-name slot) (slot-value obj slot))))

(set-pprint-dispatch 'standard-object
  (lambda (str obj)
    (format str "{~{~{~S => ~S~}~^, ~}}"
            (slot-values obj))))|#

;;; Safety first!
(declaim (optimize (safety 3) (debug 3) (speed 0)))
