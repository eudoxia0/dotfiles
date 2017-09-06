#!/usr/bin/env cl

(defun synchronize-directories (&key source parent)
  "Synchronize the contents of the `source` directory to a subdirectory of the
same name in `parent`."
  (format t "Synchronizing ~S to ~S~%" (namestring source) (namestring parent))
  (uiop:run-program (format nil "rsync -av ~S ~S --delete" (namestring source) (namestring parent))
                    :output t
                    :error-output t)
  (format nil "Directories synchronized~%"))

(defparameter +directories-to-synchronize+
  (list ".fonts"
        ".ssh"
        "backup"
        "code"
        "images"
        "library"
        "music"
        "self"
        "texmf"
        "wiki"
        "work"
        "writing"))

(defparameter +source-parent-directory+ (user-homedir-pathname)
  "The directory containing the directories we want to back up.")

(defun backup-directory ()
  (let ((username (uiop:run-program "whoami" :output '(:string :stripped t))))
    (make-pathname :directory (list :absolute "media" username "backup" "automated"))))

;;; Interface

(defun make-backup ()
  "Back up local directories to the backup store."
  (format t "Creating backups~%")
  (let ((backup-directory (backup-directory)))
    (loop for directory-name in +directories-to-synchronize+ do
      (synchronize-directories :source (merge-pathnames directory-name +source-parent-directory+)
                               :parent backup-directory))))


(let ((args uiop:*command-line-arguments*))
  (if args
      (cond ((string= (first args) "make-backup")
             (make-backup))
            (t
             (format t "Unknown command~%")))
      (format t "Missing command~%")))
