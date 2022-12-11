;;;;
;;;; Common components for various system configurations.
;;;;
(in-package :cl-user)
(defpackage common
  (:use :cl :lcm)
  (:export :apt-one-way-component
           :make-file-component
           :native-path))
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

(defcomponent file-component (component)
  ((path :reader component-path
         :initarg :path
         :type pathname
         :documentation "The path to the file.")
   (contents :reader component-contents
             :initarg :contents
             :type string
             :documentation "The file contents."))
  :documentation "A component to create a file."

  :appliedp (((component file-component))
             "Applied when the file exists and its contents are identical to the string."
             (with-slots (path contents) component
               (and (probe-file path)
                    (string= (uiop:read-file-string path) contents))))

  :apply (((component file-component))
          (with-slots (path contents) component
            (with-open-file (stream path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-sequence contents stream))))

  :unapply (((component file-component))
            (delete-file (component-path component))))

(defun make-file-component (&key title source target)
  (make-instance 'file-component
                 :title title
                 :path target
                 :contents (uiop:read-file-string source)))

(defun native-path (spec)
  (uiop:native-namestring spec))
