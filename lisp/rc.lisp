;(setf *print-case* :downcase)

;;; Load Quicklisp

#-quicklisp
(load (merge-pathnames ".quicklisp/setup"
                       (user-homedir-pathname)))

;;; Better printing for hash tables

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))

;;; Safety first!

(declaim (optimize (safety 3) (debug 3) (speed 0)))

;;; SBCL-specific

#+sbcl (sb-ext:restrict-compiler-policy 'debug 3)
