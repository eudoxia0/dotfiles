#!/usr/bin/env cl

(defun synchronize-directories (&key source parent)
  "Synchronize the contents of the `source` directory to a subdirectory of the
same name in `parent`."
  (format t "Synchronizing ~S to ~S" (namestring source) (namestring parent))
  (uiop:run-program (format nil "rsync -av ~S ~S --delete" (namestring source) (namestring parent))
                    :output t
                    :error-output t))

(defparameter +directories-to-synchronize+
  (list "code" "images" "writing" "self" "backup" "library" "music" "work"
        "wiki" ".ssh" ".purple" ".fonts"))

(defparameter +source-parent-directory+ (user-homedir-pathname)
  "The directory containing the directories we want to back up.")

(defun backup-directory ()
  (let ((username (uiop:run-program "whoami" :output '(:string :stripped t))))
    (make-pathname :directory (list :absolute "media" username "backup"))))

;;; Interface

(defun make-backup ()
  "Back up local directories to the backup store."
  (let ((backup-directory (backup-directory)))
    (loop for directory-name in +directories-to-synchronize+ do
      (synchronize-directories :source (merge-pathnames directory-name +source-parent-directory+)
                               :parent backup-directory))))

(defun restore-backup ()
  "Restore the backup."
  (let ((backup-directory (backup-directory)))
    (loop for directory-name in +directories-to-synchronize+ do
      (synchronize-directories :source (merge-pathnames directory-name backup-directory)
                               :parent +source-parent-directory+))))
