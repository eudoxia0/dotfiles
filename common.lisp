;;;;
;;;; Common components for various system configurations.
;;;;
(in-package :cl-user)
(defpackage common
  (:use :cl :lcm)
  (:export :apt-one-way-component))
(in-package :common)

;;;
;;; APT Component
;;;

(defun apt-installed-p (package)
  (multiple-value-bind (a b code)
      (uiop:run-program (format nil "apt-cache show ~A" package) :ignore-error-status t)
    (declare (ignore a b))
    (= code 0)))

(defcomponent apt-one-way-component (component)
  ((packages :reader packages
             :initarg :packages
             :type list
             :documentation "A list of package names."))
  :documentation "A component to install APT package. One way because
  unapplying does not uninstall the packages, for speed."
  :appliedp ((component)
             "Triggers when any package is not installed."
             (some #'(lambda (pkg) (not (apt-installed-p pkg))) (packages component)))
  :apply ((component)
          "Install all packages."
          (let ((cmd (format nil "sudo apt-get install -y ~{~A ~}" (packages component))))
            (uiop:run-program cmd)))
  :unapply ((component)
            "Does nothing."
            nil))
