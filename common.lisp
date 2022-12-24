;;;;
;;;; Common components for various system configurations.
;;;;
(in-package :cl-user)
(defpackage common
  (:use :cl :lcm :lcm.builtin)
  (:export :apt-one-way-component
           :make-file-component
           :native-path))
(in-package :common)

;;;
;;; APT Component
;;;

(defun apt-installed-p (package)
  (multiple-value-bind (a b code)
      (uiop:run-program (format nil "dpkg -s ~A" package) :ignore-error-status t)
    (declare (ignore a b))
    (= code 0)))

(defcomponent apt-one-way-component (component)
  ((packages :reader packages
             :initarg :packages
             :type list
             :documentation "A list of package names."))
  :documentation "A component to install APT package. One way because
  unapplying does not uninstall the packages, for speed."
  :appliedp (((component apt-one-way-component))
             "Applied when all packages are installed."
             (every #'apt-installed-p (packages component)))
  :apply (((component apt-one-way-component))
          "Install all packages."
          (let ((cmd (format nil "sudo apt-get install -y ~{~A ~}" (packages component))))
            (uiop:run-program cmd)))
  :unapply (((component apt-one-way-component))
            "Does nothing."
            nil))

;;;
;;; File Component
;;;

(defun make-file-component (&key title source target (executable nil))
  (make-instance 'file-component
                 :title title
                 :path target
                 :contents (uiop:read-file-string source)
                 :executable executable))

(defun native-path (spec)
  (uiop:native-namestring spec))
