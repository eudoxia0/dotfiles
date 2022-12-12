;;;;
;;;; Common components for various system configurations.
;;;;
(in-package :cl-user)
(defpackage common
  (:use :cl :lcm)
  (:import-from :org.shirakumo.file-attributes
                :attributes
                :encode-attributes)
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

(defun chmod+x (pathname)
  (let ((attrs (list :other-execute nil
                     :other-write nil
                     :other-read t
                     :group-execute nil
                     :group-write t
                     :group-read t
                     :owner-execute t ;; here
                     :owner-write t
                     :owner-read t
                     :sticky nil
                     :set-group nil
                     :set-user nil
                     :fifo nil
                     :device nil
                     :directory nil
                     :normal t
                     :link nil
                     :socket nil)))
    (setf (attributes pathname) (encode-attributes attrs))))


(defcomponent file-component (component)
  ((path :reader component-path
         :initarg :path
         :type pathname
         :documentation "The path to the file.")
   (contents :reader component-contents
             :initarg :contents
             :type string
             :documentation "The file contents.")
   (executable :reader component-executable
               :initarg :executable
               :initform nil
               :type boolean
               :documentation "Whether the resulting file should be executable."))
  :documentation "A component to create a file."

  :appliedp (((component file-component))
             "Applied when the file exists and its contents are identical to the string."
             (with-slots (path contents) component
               (and (probe-file path)
                    (string= (uiop:read-file-string path) contents))))

  :apply (((component file-component))
          (with-slots (path contents executable) component
            ;; Ensure the parent directories exist.
            (ensure-directories-exist (uiop:pathname-directory-pathname path))
            ;; Write the file.
            (with-open-file (stream path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-string contents stream))
            ;; chmod+x if needd
            (when executable
              (chmod+x path))))

  :unapply (((component file-component))
            (delete-file (component-path component))))

(defun make-file-component (&key title source target (executable nil))
  (make-instance 'file-component
                 :title title
                 :path target
                 :contents (uiop:read-file-string source)
                 :executable executable))

(defun native-path (spec)
  (uiop:native-namestring spec))
